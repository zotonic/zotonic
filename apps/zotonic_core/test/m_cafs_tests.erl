%% @doc CAFS integration tests run against the disposable testsandbox database.
-module(m_cafs_tests).
-export([observe_filestore/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").

path_validation_test() ->
    Hash = binary:copy(<<"a">>, 64),
    ?assertEqual(Hash, m_cafs:path_hash(m_cafs:path(Hash))),
    ?assertEqual(undefined, m_cafs:path_hash(<<"archive/cafs/sha256/ab/cd/", Hash/binary>>)),
    ?assertError({badmatch, false}, m_cafs:path(binary:copy(<<"/">>,64))).

trigger_install_test_() -> {timeout, 60, fun trigger_install/0}.
trigger_install() ->
    %% Roll back deliberate old/missing definitions, including on assertion failure.
    Result = z_db:transaction(fun(C) ->
        ok = m_cafs:install(C),
        Snapshot = fun() -> z_db:q_row(
            "select p.ctid::text, t.oid, t.ctid::text
             from pg_proc p join pg_trigger t on t.tgfoid = p.oid
             where p.oid = to_regprocedure('cafs_medium_retire()')
               and t.tgrelid = 'medium'::regclass and t.tgname = 'cafs_medium_retire'", C)
        end,
        Original = Snapshot(),
        ok = m_cafs:install(C),
        ?assertEqual(Original, Snapshot()),
        [] = z_db:q("create or replace function cafs_medium_retire() returns trigger
            as $$ begin return null; end; $$ language plpgsql", C),
        ok = m_cafs:install(C),
        {_, TriggerOid, TriggerTuple} = Original,
        ?assertMatch({_, TriggerOid, TriggerTuple}, Snapshot()),
        Repaired = Snapshot(),
        ok = m_cafs:install(C),
        ?assertEqual(Repaired, Snapshot()),
        [] = z_db:q("drop trigger cafs_medium_retire on medium", C),
        ok = m_cafs:install(C),
        ?assertNotEqual(Repaired, Snapshot()),
        [] = z_db:q("alter table medium disable trigger cafs_medium_retire", C),
        ok = m_cafs:install(C),
        ?assertEqual(<<"O">>, z_db:q1("select tgenabled::text from pg_trigger
            where tgrelid = 'medium'::regclass and tgname = 'cafs_medium_retire'", C)),
        throw({rollback, trigger_install_checked})
    end, context()),
    ?assertEqual({rollback, trigger_install_checked}, Result).

storage_test_() -> {timeout, 120, fun storage/0}.
storage() ->
    C = context(),
    File = tempfile(),
    try
        ok = m_cafs:install(C),
        {ok, #{hash := Hash} = Stage} = m_cafs:stage_file(File, C),
        Name = name(),
        ok = m_cafs:attach(Name, Stage, C),
        %% Reinstall is the same schema path used on upgrade and is idempotent.
        ok = m_cafs:install(C),
        ?assertEqual({ok, Hash}, m_cafs:lookup(Name, C)),
        Copy = name(),
        ok = m_cafs:clone_filename(Name, Copy, C),
        ?assertEqual({ok, Hash}, m_cafs:lookup(Copy, C)),
        Physical = z_path:files_subdir(m_cafs:path(Hash), C),
        %% A mapped remote-only source can be cloned without reading its bytes.
        ok = file:delete(Physical),
        Copy2 = name(),
        ok = m_cafs:clone_filename(Name, Copy2, C),
        ?assertEqual({ok, Hash}, m_cafs:lookup(Copy2, C)),
        {ok, Stage2} = m_cafs:stage_file(File, C),
        ok = m_cafs:release([{Name, Stage2}], C),
        ?assertEqual(file:read_file(File), m_cafs:with_local_file(Hash, fun file:read_file/1, C)),
        lists:foreach(fun(N) -> ok = m_cafs:retire_filename(N, C) end, [Name,Copy,Copy2]),
        expire(Hash, C),
        ok = m_cafs:maintenance(C),
        ?assertEqual(undefined, z_db:q1("select hash from cafs where hash=$1", [Hash], C)),
        ?assertNot(filelib:is_regular(Physical))
    after file:delete(File) end.

rollback_test_() -> {timeout, 60, fun rollback/0}.
rollback() ->
    C = context(), File = tempfile(), Name = name(),
    try
        {ok, #{hash := Hash, token := Token} = Stage} = m_cafs:stage_file(File,C),
        ?assertMatch({rollback, _}, z_db:transaction(fun(Ctx) ->
            ok = m_cafs:attach(Name,Stage,Ctx),
            throw({rollback, deliberate})
        end,C)),
        ?assertEqual({error,enoent},m_cafs:lookup(Name,C)),
        ?assertEqual(1,z_db:q1("select count(*) from cafs_pin where hash=$1 and owner=$2",[Hash,Token],C)),
        ok = m_cafs:attach(Name,Stage,C),
        cleanup(Hash,C)
    after file:delete(File) end.

concurrent_derivative_test_() -> {timeout, 120, fun concurrent_derivative/0}.
concurrent_derivative() ->
    C = context(), File = tempfile(), Output = tempfile(),
    try
        {ok, #{hash := Hash} = Stage} = m_cafs:stage_file(File,C),
        ok = m_cafs:attach(name(),Stage,C),
        Parent = self(),
        Generate = fun() ->
            Parent ! generated,
            timer:sleep(100),
            m_cafs:stage_file(Output,C)
        end,
        Workers = [spawn_monitor(fun() ->
            receive go -> ok end,
            Parent ! {self(),m_cafs:derivative(Hash,[{width,100}],1,Generate,C)}
        end) || _ <- lists:seq(1,6)],
        [P ! go || {P,_} <- Workers],
        Results = [receive {P,R} ->
            receive {'DOWN',Ref,process,P,normal} -> ok after 10000 -> error(worker_failed) end, R
            after 30000 -> error(timeout) end || {P,Ref} <- Workers],
        [{ok, OutHash}] = lists:usort(Results),
        receive generated -> ok after 1000 -> error(not_generated) end,
        receive generated -> error(generated_twice) after 100 -> ok end,
        %% Renderer changes cannot reuse the previous generation record.
        ?assertEqual({ok,OutHash},m_cafs:derivative(Hash,[{width,100}],2,Generate,C)),
        receive generated -> ok after 1000 -> error(version_not_generated) end,
        cleanup(Hash,C), cleanup(OutHash,C)
    after file:delete(File),file:delete(Output) end.

retention_test_() -> {timeout, 60, fun retention/0}.
retention() ->
    C=context(), File=tempfile(), Name=name(), Copy=name(),
    try
        {ok,#{hash:=Hash}=Stage}=m_cafs:stage_file(File,C),
        ok=m_cafs:attach(Name,Stage,C),
        ok=m_cafs:clone_filename(Name,Copy,C),
        ok=m_cafs:retire_filename(Name,C),
        expire(Hash,C), ok=m_cafs:maintenance(C),
        ?assertEqual({ok,Hash},m_cafs:lookup(Copy,C)),
        ok=m_cafs:pin(Hash,<<"test-pin">>,C),
        ok=m_cafs:retire_filename(Copy,C),
        expire(Hash,C), ok=m_cafs:maintenance(C),
        ?assertEqual(Hash,z_db:q1("select hash from cafs where hash=$1",[Hash],C)),
        ok=m_cafs:unpin(Hash,<<"test-pin">>,C),
        expire(Hash,C), ok=m_cafs:maintenance(C),
        ?assertEqual(undefined,z_db:q1("select hash from cafs where hash=$1",[Hash],C))
    after file:delete(File) end.

deleting_test_() -> {timeout,60,fun deleting/0}.
deleting() ->
    C=context(), File=tempfile(),
    try
        {ok,#{hash:=Hash}=Stage}=m_cafs:stage_file(File,C),
        Name=name(), ok=m_cafs:attach(Name,Stage,C),
        z_db:q("update cafs set state='deleting' where hash=$1",[Hash],C),
        ?assertEqual({error,deleting},m_cafs:clone_filename(Name,name(),C)),
        %% Without a backend confirmation, retain the deletion tombstone.
        ?assertEqual({error,deleting},m_cafs:stage_file(File,C)),
        ok = z_notifier:observe(filestore, {mod_filestore, observe_filestore, []}, self(), 1, C),
        {ok, Revived} = m_cafs:stage_file(File,C),
        ok = m_cafs:release([{Name, Revived}],C),
        z_db:q("update cafs set state='ready' where hash=$1",[Hash],C),
        cleanup(Hash,C)
    after
        z_notifier:detach(filestore, self(), C),
        file:delete(File)
    end.

media_roundtrip_test_() -> {timeout,120,fun media_roundtrip/0}.
media_roundtrip() ->
    C=context(), File=tempfile(),
    try
        {ok,Id}=m_media:insert_file(#upload{filename= <<"cafs-test.txt">>,tmpfile=File},C),
        #{<<"filename">>:=Name,<<"digest">>:=Hash}=m_media:get(Id,C),
        ?assertEqual({ok,Hash},m_cafs:lookup(Name,C)),
        ?assertNot(filelib:is_regular(z_media_archive:abspath(Name,C))),
        {ok,CopyId}=m_rsc:duplicate(Id,#{},C),
        #{<<"filename">>:=Copy}=m_media:get(CopyId,C),
        ?assertEqual({ok,Hash},m_cafs:lookup(Copy,C)),
        {ok,Info}=z_file_request:lookup_file(Copy,[archive],[],C),
        {ok,Local}=z_file_request:content_file(Info,C),
        ?assertEqual(file:read_file(File),file:read_file(Local)),
        ok=m_rsc:delete(Id,C),
        ?assertEqual({ok,Hash},m_cafs:lookup(Copy,C)),
        ok=m_rsc:delete(CopyId,C),
        cleanup(Hash,C)
    after file:delete(File) end.

image_preview_test_() -> {timeout,120,fun image_preview/0}.
image_preview() ->
    C=context(),
    File=filename:join(code:priv_dir(zotonic_site_testsandbox),"files/archive/koe.jpg"),
    {ok,Data}=file:read_file(File),
    {ok,Id}=m_media:insert_file(#upload{filename= <<"koe.jpg">>,data=Data},
        #{<<"is_published">> => false}, [], C),
    {ok,CopyId}=m_rsc:duplicate(Id,#{},C),
    try
        #{<<"digest">>:=Hash}=m_media:get(Id,C),
        Paths=[begin
            {ok,Url}=z_media_tag:url(RId,[{width,31}],C),
            [_,Path]=binary:split(Url,<<"/image/">>),
            z_url:url_decode(Path)
        end || RId <- [Id,CopyId]],
        Files=[begin
            {ok,Info}=z_file_request:lookup_file(Path,C),
            ?assert(z_file_request:is_visible(Info,C)),
            {ok,Local}=z_file_request:content_file(Info,C),
            Local
        end || Path <- Paths],
        ?assertEqual(1,length(lists:usort(Files))),
        ?assertEqual(1,z_db:q1("select count(*) from cafs_derivative where source_hash=$1",[Hash],C)),
        {ok,#{<<"width">>:=31}}=z_media_identify:identify(hd(Files),C),
        %% Distinct requesting resources retain distinct ACL identities.
        {ok,PublicInfo}=z_file_request:lookup_file(hd(Paths),C),
        ?assertNot(z_file_request:is_visible(PublicInfo,z_context:new(zotonic_site_testsandbox)))
    after m_rsc:delete(Id,C),m_rsc:delete(CopyId,C) end.

upgrade_schema_test_() -> {timeout,60,fun upgrade_schema/0}.
upgrade_schema() ->
    C=context(),
    Sql="select table_name,column_name,data_type,is_nullable,column_default
        from information_schema.columns where table_schema=current_schema()
        and table_name in ('cafs','filename_cafs','cafs_pin','cafs_derivative')
        order by table_name,ordinal_position",
    Fresh=z_db:q(Sql,C),
    ?assert(Fresh=/=[]),
    Result=z_db:transaction(fun(Ctx) ->
        z_db:q("drop table cafs_derivative,filename_cafs,cafs_pin,cafs",Ctx),
        ok=m_cafs:install(Ctx),
        ?assertEqual(Fresh,z_db:q(Sql,Ctx)),
        ok=m_cafs:install(Ctx),
        ?assertEqual(Fresh,z_db:q(Sql,Ctx)),
        {rollback,tested}
    end,C),
    ?assertEqual({rollback,tested},Result).

concurrent_upload_test_() -> {timeout,60,fun concurrent_upload/0}.
concurrent_upload() ->
    C=context(), File=tempfile(), Parent=self(),
    try
        Workers=[spawn_monitor(fun() ->
            receive go -> ok end,
            {ok,#{hash:=Hash}=Stage}=m_cafs:stage_file(File,C),
            Name=name(), ok=m_cafs:attach(Name,Stage,C),
            Parent ! {self(),Hash}
        end) || _ <- lists:seq(1,6)],
        [P ! go || {P,_} <- Workers],
        Hashes=[receive {P,H} ->
            receive {'DOWN',Ref,process,P,normal} -> ok after 10000 -> error(worker_failed) end,H
        after 30000 -> error(timeout) end || {P,Ref} <- Workers],
        [Hash]=lists:usort(Hashes),
        ?assertEqual(6,z_db:q1("select count(*) from filename_cafs where hash=$1",[Hash],C)),
        ?assertEqual(file:read_file(File),m_cafs:with_local_file(Hash,fun file:read_file/1,C)),
        cleanup(Hash,C)
    after file:delete(File) end.

stale_medium_test_() -> {timeout,60,fun stale_medium/0}.
stale_medium() ->
    C=context(), File=tempfile(),
    try
        {ok,Id}=m_media:insert_file(#upload{filename= <<"original.txt">>,tmpfile=File},C),
        Before=m_media:get(Id,C),
        ?assertEqual({error,stale_medium},m_media:replace_file(
            #upload{filename= <<"late.txt">>,tmpfile=File},Id,#{},#{},
            [{expected_medium,{<<"video_processing_nr">>,<<"obsolete">>}}],C)),
        ?assertEqual(Before,m_media:get(Id,C)),
        m_rsc:delete(Id,C)
    after file:delete(File) end.

lazy_promotion_test_() -> {timeout,60,fun lazy_promotion/0}.
lazy_promotion() ->
    C=context(), Name=name(),
    Local=z_media_archive:abspath(Name,C),
    ok=z_filelib:ensure_dir(Local),
    ok=file:write_file(Local,<<"Legacy file ",(z_ids:id())/binary>>),
    {ok,Id}=m_rsc:insert(#{<<"category">>=>media},C),
    ok=m_media:replace(Id,#{<<"filename">>=>Name,<<"mime">>=><<"text/plain">>,
        <<"is_deletable_file">>=>true},C),
    try
        ?assertEqual({error,enoent},m_cafs:lookup(Name,C)),
        {ok,CopyId}=m_rsc:duplicate(Id,#{},C),
        {ok,Hash}=m_cafs:lookup(Name,C),
        #{<<"filename">>:=CopyName}=m_media:get(CopyId,C),
        ?assertEqual({ok,Hash},m_cafs:lookup(CopyName,C)),
        ?assert(filelib:is_regular(Local)), % Keep the legacy backup reference.
        ok=m_rsc:delete(CopyId,C)
    after m_rsc:delete(Id,C),file:delete(Local) end.

orphan_publication_test_() -> {timeout,60,fun orphan_publication/0}.
orphan_publication() ->
    C=context(), File=tempfile(),
    try
        {ok,Hash}=z_crypto:hex_sha2_file(File),
        Physical=z_path:files_subdir(m_cafs:path(Hash),C),
        ok=z_filelib:ensure_dir(Physical),
        {ok,_}=file:copy(File,Physical),
        {ok,Info}=file:read_file_info(Physical,[{time,posix}]),
        ok=file:write_file_info(Physical,Info#file_info{mtime=erlang:system_time(second)-36*86400},[{time,posix}]),
        <<Prefix:2/binary,_/binary>>=Hash,
        ok=z_cafs_file:reconcile_prefix(Prefix,C),
        ?assertEqual(Hash,z_db:q1("select hash from cafs where hash=$1",[Hash],C)),
        ok=m_cafs:maintenance(C),
        ?assertNot(filelib:is_regular(Physical))
    after file:delete(File) end.

context() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = m_filestore:install(install, C),
    ok = z_notifier:observe(filestore, {?MODULE, observe_filestore, []}, self(), 10, C),
    C.

%% Exercise backend deletion without enabling background uploads in these tests.
observe_filestore(#filestore{action=delete_status}=Request, Context) ->
    mod_filestore:observe_filestore(Request, Context);
observe_filestore(_, _) -> undefined.
name() -> <<"cafs-test/",(z_ids:id())/binary,".txt">>.
tempfile() ->
    File=z_tempfile:new(),
    ok=file:write_file(File,<<"CAFS test ",(z_ids:id())/binary>>),
    File.
expire(Hash,C) -> z_db:q("update cafs set gc_after=now()-interval '1 day' where hash=$1",[Hash],C).
cleanup(Hash,C) ->
    z_db:q("delete from filename_cafs where hash=$1",[Hash],C),
    z_db:q("delete from cafs_pin where hash=$1",[Hash],C),
    expire(Hash,C), m_cafs:maintenance(C).
