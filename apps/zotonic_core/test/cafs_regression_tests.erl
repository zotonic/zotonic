%% Copyright 2026 Marc Worrell. SPDX-License-Identifier: Apache-2.0
%% @doc Regression coverage for the CAFS lifecycle and legacy-file audit.
-module(cafs_regression_tests).
-export([observe_filestore/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").

archive_acl_test_() -> {timeout, 60, fun archive_acl/0}.
archive_acl() ->
    C = context(),
    {ok, Id} = m_media:insert_file(#upload{filename= <<"private.txt">>, data=unique()},
        #{<<"is_published">> => false}, [], C),
    try
        #{<<"filename">> := Name} = m_media:get(Id, C),
        {ok, _} = z_file_request:lookup_file(Name, [archive], [], C),
        {ok, Info} = z_file_request:lookup_file(Name, C),
        ?assertNot(z_file_request:is_visible(Info, anonymous())),
        ?assertNotEqual(z_file_entry:where(Name, C), z_file_entry:where(Name, [archive], [], C)),
        %% Public-first lookups and a public alias must retain their own ACLs.
        {ok, Copy} = m_rsc:duplicate(Id, #{<<"is_published">> => true}, C),
        try
            #{<<"filename">> := CopyName} = m_media:get(Copy, C),
            {ok, Public} = z_file_request:lookup_file(CopyName, C),
            {ok, _} = z_file_request:lookup_file(CopyName, [archive], [], C),
            ?assert(z_file_request:is_visible(Public, anonymous())),
            {ok, Private} = z_file_request:lookup_file(Name, C),
            ?assertNot(z_file_request:is_visible(Private, anonymous()))
        after m_rsc:delete(Copy, C) end
    after m_rsc:delete(Id, C) end.

legacy_checksum_acl_test_() -> {timeout, 60, fun legacy_checksum_acl/0}.
legacy_checksum_acl() ->
    C = context(),
    Bytes = unique(), Hash = digest(Bytes),
    lists:foreach(fun(Digest) ->
        Name = <<"audit/", (unique())/binary, ".txt">>,
        File = z_media_archive:abspath(Name, C),
        ok = z_filelib:ensure_dir(File), ok = file:write_file(File, Bytes),
        {ok, Source} = m_rsc:insert(#{<<"category">> => media, <<"is_published">> => false}, C),
        {ok, Dest} = m_rsc:insert(#{<<"category">> => media}, C),
        try
            ok = m_media:replace(Source, #{<<"filename">> => Name, <<"mime">> => <<"text/plain">>,
                <<"is_deletable_file">> => true, <<"digest">> => Digest}, C),
            ok = m_media:copy(Source, Dest, C),
            ?assert(lists:member(z_file_entry:where(Name, [archive], [], C),
                m_cafs:readers(<<"archive/", Name/binary>>, C))),
            ?assertEqual(Hash, maps:get(<<"digest">>, m_media:get(Dest, C))),
            {ok, Info} = z_file_request:lookup_file(Name, C),
            ?assertNot(z_file_request:is_visible(Info, anonymous()))
        after m_rsc:delete(Source, C), m_rsc:delete(Dest, C), file:delete(File) end
    end, [undefined, Hash, binary:copy(<<"0">>, 64)]).

literal_prefix_test_() -> {timeout, 60, fun literal_prefix/0}.
literal_prefix() ->
    C = context(), ok = m_filestore:install(install, C),
    Stem = <<"preview/", (unique())/binary>>,
    P1 = <<Stem/binary, "a_b%\\.jpg(31x).jpg">>,
    P2 = <<Stem/binary, "axbZZ.jpg(31x).jpg">>,
    {ok, I1} = store(P1, C), {ok, I2} = store(P2, C),
    try
        ?assertEqual({ok, 1}, m_filestore:mark_deleted({prefix, <<Stem/binary, "a_b%\\.jpg(">>}, C)),
        ?assertEqual(false, z_db:q1("select is_deleted from filestore where id=$1", [I2], C)),
        Since = z_db:q1("select deleted from filestore where id=$1", [I1], C),
        ?assertEqual({error, enoent}, m_filestore:mark_deleted(P1, C)),
        ?assertEqual(Since, z_db:q1("select deleted from filestore where id=$1", [I1], C))
    after z_db:q("delete from filestore where id in ($1,$2)", [I1,I2], C) end.

delete_status_test_() -> {timeout, 60, fun delete_status/0}.
delete_status() ->
    C = context(),
    Prefix = <<"archive/", (unique())/binary, "_%/">>,
    Path = <<Prefix/binary, "file">>,
    try
        ok = z_notifier:detach(filestore, self(), C),
        ?assertEqual({error, filestore_unavailable}, z_file_request:delete_remote(Path, C)),
        ok = m_config:set_value(site, filestore_used, false, C),
        ?assertEqual(absent, z_file_request:delete_remote(Path, C)),
        ok = m_config:set_value(site, filestore_used, true, C),
        ok = z_notifier:observe(filestore, {?MODULE, observe_filestore, []}, self(), 10, C),
        ?assertEqual(absent, z_file_request:delete_remote(Path, C)),
        ok = m_filestore:queue(Path, #{}, C),
        ?assertEqual(pending, z_file_request:delete_remote(Path, C)),
        ?assertEqual(pending, z_file_request:delete_remote({prefix, Prefix}, C)),
        z_db:q("delete from filestore_queue where path=$1", [Path], C),
        {ok, Id} = store(Path, C),
        ok = m_config:set_value(mod_filestore, is_read_only, true, C),
        ?assertEqual({error, read_only}, z_file_request:delete_remote(Path, C)),
        ?assertEqual(false, z_db:q1("select is_deleted from filestore where id=$1", [Id], C)),
        ok = m_config:set_value(mod_filestore, is_read_only, false, C),
        ?assertEqual(pending, z_file_request:delete_remote({prefix, Prefix}, C)),
        Since = z_db:q1("select deleted from filestore where id=$1", [Id], C),
        ?assertEqual(pending, z_file_request:delete_remote(Path, C)),
        ?assertEqual(Since, z_db:q1("select deleted from filestore where id=$1", [Id], C)),
        ok = m_filestore:purge_deleted(Id, C),
        ?assertEqual(absent, z_file_request:delete_remote({prefix, Prefix}, C))
    after
        m_config:set_value(site, filestore_used, true, C),
        m_config:set_value(mod_filestore, is_read_only, false, C),
        z_db:q("delete from filestore_queue where path=$1", [Path], C),
        z_db:q("delete from filestore where path=$1", [Path], C)
    end.

cancel_delete_test_() -> {timeout, 60, fun cancel_delete/0}.
cancel_delete() ->
    C = context(), ok = m_filestore:install(install, C),
    Path = <<"archive/", (unique())/binary>>,
    Request = #filestore{action=cancel_delete, path=Path},
    ?assertEqual(true, mod_filestore:observe_filestore(Request, C)),
    {ok, Id} = store(Path, C),
    try
        {ok, 1} = m_filestore:mark_deleted(Path, C),
        Result = z_db:transaction(fun(Ctx) ->
            true = mod_filestore:observe_filestore(Request, Ctx),
            false = z_db:q1("select is_deleted from filestore where id=$1", [Id], Ctx),
            throw({rollback, cancellation_check})
        end, C),
        ?assertEqual({rollback, cancellation_check}, Result),
        ?assertEqual(true, z_db:q1("select is_deleted from filestore where id=$1", [Id], C)),
        ?assertEqual(true, mod_filestore:observe_filestore(Request, C)),
        ?assertEqual(false, z_db:q1("select is_deleted from filestore where id=$1", [Id], C)),
        {ok, 1} = m_filestore:mark_deleted(Path, C),
        ?assert(m_filestore:claim_delete(Id, C)),
        ?assertEqual(false, mod_filestore:observe_filestore(Request, C))
    after z_db:q("delete from filestore where id=$1", [Id], C) end.

forbidden_delete_test_() -> {timeout, 60, fun forbidden_delete/0}.
forbidden_delete() ->
    C = context(), ok = m_filestore:install(install, C),
    Path = <<"archive/", (unique())/binary>>,
    {ok, Id} = store(Path, C),
    try
        {ok, 1} = m_filestore:mark_deleted(Path, C),
        ?assert(m_filestore:claim_delete(Id, C)),
        ok = mod_filestore:delete_ready(Id, Path, C, make_ref(), {error, forbidden}),
        ?assertEqual(true, z_db:q1("select delete_started from filestore where id=$1", [Id], C)),
        ?assertEqual({error, enoent}, m_filestore:clear_deleted(Id, C)),
        ok = mod_filestore:delete_ready(Id, Path, C, make_ref(), {error, enoent}),
        ?assertEqual(undefined, z_db:q1("select id from filestore where id=$1", [Id], C))
    after z_db:q("delete from filestore where id=$1", [Id], C) end.

blocked_gc_test_() -> {timeout, 120, fun blocked_gc/0}.
blocked_gc() ->
    C = context(), ok = m_filestore:install(install, C),
    Hashes = [digest(unique()) || _ <- lists:seq(1, 100)],
    Candidate = digest(unique()),
    try
        lists:foreach(fun(Hash) ->
            z_db:q("insert into cafs (hash,size,state,upload_pending,gc_after)
                values ($1,1,'deleting',false,'2000-01-01')", [Hash], C),
            {ok, _} = store(m_cafs:path(Hash), C),
            {ok, 1} = m_filestore:mark_deleted(m_cafs:path(Hash), C)
        end, Hashes),
        z_db:q("insert into cafs (hash,size,upload_pending,gc_after)
            values ($1,1,false,'2001-01-01')", [Candidate], C),
        ok = m_cafs:maintenance(C), ok = m_cafs:maintenance(C),
        ?assertEqual(undefined, z_db:q1("select state from cafs where hash=$1", [Candidate], C))
    after
        lists:foreach(fun(Hash) ->
            z_db:q("delete from filestore where path=$1", [m_cafs:path(Hash)], C),
            z_db:q("delete from cafs where hash=$1", [Hash], C)
        end, [Candidate | Hashes])
    end.

pending_reupload_test_() -> {timeout, 60, fun pending_reupload/0}.
pending_reupload() ->
    C = context(), ok = m_filestore:install(install, C),
    Bytes = unique(),
    {ok, Source} = m_media:insert_file(#upload{filename= <<"source.txt">>, data=Bytes}, C),
    #{<<"digest">> := Hash} = m_media:get(Source, C),
    Path = m_cafs:path(Hash), {ok, Fid} = store(Path, C),
    ok = z_notifier:observe(filestore, {mod_filestore, observe_filestore, []}, self(), 1, C),
    try
        z_db:q("update cafs set state='deleting' where hash=$1", [Hash], C),
        {ok, 1} = m_filestore:mark_deleted(Path, C),
        %% Retention/read-only pending deletes have not been dispatched: revive.
        {ok, Copy} = m_media:insert_file(#upload{filename= <<"copy.txt">>, data=Bytes}, C),
        ok = m_rsc:delete(Copy, C),
        ?assertNot(m_filestore:claim_delete(Fid, C)),
        z_db:q("update cafs set state='deleting' where hash=$1", [Hash], C),
        {ok, 1} = m_filestore:mark_deleted(Path, C),
        ?assert(m_filestore:claim_delete(Fid, C)),
        %% Once dispatched, return a controlled error and preserve the resource.
        ?assertEqual({error, deleting}, m_media:replace_file(
            #upload{filename= <<"retry.txt">>, data=Bytes}, Source, C)),
        ?assertEqual(Hash, maps:get(<<"digest">>, m_media:get(Source, C))),
        ok = mod_filestore:delete_ready(Fid, Path, C, make_ref(), ok),
        {ok, Retry} = m_media:insert_file(#upload{filename= <<"retry.txt">>, data=Bytes}, C),
        ok = m_rsc:delete(Retry, C)
    after
        z_db:q("update cafs set state='ready' where hash=$1", [Hash], C),
        z_db:q("delete from filestore where id=$1", [Fid], C),
        z_notifier:detach(filestore, self(), C),
        m_rsc:delete(Source, C)
    end.

preview_metadata_test_() -> {timeout, 120, fun preview_metadata/0}.
preview_metadata() ->
    C = context(),
    File = filename:join(code:priv_dir(zotonic_site_testsandbox), "files/archive/koe.jpg"),
    {ok, Bytes} = file:read_file(File),
    {ok, Source} = m_media:insert_file(#upload{filename= <<"koe.jpg">>, data=Bytes}, C),
    {ok, Dest} = m_rsc:duplicate(Source, #{}, C),
    try
        Medium = m_media:get(Dest, C),
        ok = m_media:replace(Dest, Medium#{<<"orientation">> => 6}, C),
        Outputs = [preview(Id, C) || Id <- [Source, Dest]],
        ?assertEqual(2, length(lists:usort(Outputs)))
    after
        #{<<"digest">> := Hash} = m_media:get(Source, C),
        lists:foreach(fun({Ops, Version}) ->
            case binary_to_term(Ops) of
                {_, [{width,37}], _} -> z_db:q("delete from cafs_derivative where source_hash=$1
                    and operations=$2 and renderer_version=$3", [Hash,Ops,Version], C);
                _ -> ok
            end
        end, z_db:q("select operations,renderer_version from cafs_derivative where source_hash=$1", [Hash], C)),
        m_rsc:delete(Source, C), m_rsc:delete(Dest, C)
    end.

legacy_cleanup_test_() -> {timeout, 120, fun legacy_cleanup/0}.
legacy_cleanup() ->
    C = context(), ok = z_media_cleanup_server:install(C),
    Name = <<"audit/", (unique())/binary, ".txt">>,
    File = z_media_archive:abspath(Name, C),
    ok = z_filelib:ensure_dir(File),
    %% A directory where a file was expected forces a real unlink failure.
    ok = file:make_dir(File),
    z_db:q("insert into medium_deleted(filename,deleted) values ($1,now()-interval '6 weeks')", [Name], C),
    try
        cleanup(C),
        ?assertEqual(true, z_db:q1("select retry_after>now() from medium_deleted where filename=$1", [Name], C)),
        ok = file:del_dir(File), ok = file:write_file(File, <<"retained">>),
        {ok, Id} = m_rsc:insert(#{<<"category">> => media}, C),
        try
            ok = m_media:replace(Id, #{<<"filename">> => Name, <<"mime">> => <<"text/plain">>,
                <<"is_deletable_file">> => true}, C),
            retry_cleanup(Name, C),
            ?assert(filelib:is_regular(File)),
            ok = m_rsc:delete(Id, C),
            retry_cleanup(Name, C),
            ?assertNot(filelib:is_regular(File)),
            ?assertEqual(0, z_db:q1("select count(*) from medium_deleted where filename=$1
                and deleted<now()-interval '5 weeks'", [Name], C))
        after m_rsc:delete(Id, C) end
    after file:delete(File), file:del_dir(File),
        z_db:q("delete from medium_deleted where filename=$1", [Name], C) end.

remote_cleanup_retry_test_() -> {timeout, 120, fun remote_cleanup_retry/0}.
remote_cleanup_retry() ->
    C = context(), ok = m_filestore:install(install, C),
    Name = <<"audit/", (unique())/binary, ".txt">>, Path = <<"archive/", Name/binary>>,
    {ok, Fid} = store(Path, C),
    z_db:q("insert into medium_deleted(filename,deleted) values ($1,now()-interval '6 weeks')", [Name], C),
    ok = meck:new(z_notifier, [passthrough, no_link]),
    try
        ok = meck:expect(z_notifier, first, fun
            (#filestore{action=delete_status, path=P}, _) when P =:= Path -> {error, unavailable};
            (Request, Ctx) -> meck:passthrough([Request, Ctx])
        end),
        cleanup(C),
        ?assertEqual(true, z_db:q1("select retry_after>now() from medium_deleted where filename=$1", [Name], C)),
        ok = meck:expect(z_notifier, first, fun
            (#filestore{action=delete_status, path=P}=Request, Ctx) when P =:= Path ->
                mod_filestore:observe_filestore(Request, Ctx);
            (Request, Ctx) -> meck:passthrough([Request, Ctx])
        end),
        retry_cleanup(Name, C),
        ?assertEqual(1, z_db:q1("select count(*) from medium_deleted where filename=$1", [Name], C)),
        ?assertEqual(true, z_db:q1("select is_deleted from filestore where id=$1", [Fid], C)),
        ok = m_filestore:purge_deleted(Fid, C),
        retry_cleanup(Name, C),
        ?assertEqual(0, z_db:q1("select count(*) from medium_deleted where filename=$1", [Name], C))
    after
        meck:unload(z_notifier),
        z_db:q("delete from medium_deleted where filename=$1", [Name], C),
        z_db:q("delete from filestore where id=$1", [Fid], C)
    end.

filestore_fence_upgrade_test_() -> {timeout, 60, fun filestore_fence_upgrade/0}.
filestore_fence_upgrade() ->
    C = context(), ok = m_filestore:install(install, C),
    Path = <<"archive/", (unique())/binary>>, {ok, Id} = store(Path, C),
    try
        {ok, 1} = m_filestore:mark_deleted(Path, C),
        Result = z_db:transaction(fun(Ctx) ->
            z_db:q("alter table filestore drop column delete_started", Ctx),
            z_db:flush(Ctx),
            ok = m_filestore:install(upgrade, Ctx),
            ok = m_filestore:install(upgrade, Ctx),
            %% Existing deleted rows may have been dispatched by old code.
            ?assertEqual(true, z_db:q1("select delete_started from filestore where id=$1", [Id], Ctx)),
            {rollback, tested}
        end, C),
        ?assertEqual({rollback, tested}, Result)
    after z_db:flush(C), z_db:q("delete from filestore where id=$1", [Id], C) end.

null_filename_test_() -> {timeout, 60, fun null_filename/0}.
null_filename() ->
    C = context(), Name = <<"audit/", (unique())/binary, ".txt">>,
    Preview = <<Name/binary, ".jpg">>,
    {ok, Id} = m_rsc:insert(#{<<"category">> => media}, C),
    try
        ok = m_media:replace(Id, #{<<"filename">> => Name, <<"mime">> => <<"text/plain">>,
            <<"is_deletable_file">> => true, <<"preview_filename">> => Preview,
            <<"is_deletable_preview">> => true}, C),
        {ok, 1} = z_db:update(medium, Id, #{<<"filename">> => undefined, <<"preview_filename">> => undefined}, C),
        ?assertEqual(2, z_db:q1("select count(*) from medium_deleted where filename in ($1,$2)", [Name,Preview], C))
    after m_rsc:delete(Id, C) end.

cleanup(C) ->
    z_media_cleanup_server:cleanup(C),
    _ = sys:get_state(z_utils:name_for_site(z_media_cleanup_server, C), 60000),
    ok.
retry_cleanup(Name, C) ->
    z_db:q("update medium_deleted set retry_after=now() where filename=$1", [Name], C),
    cleanup(C).
preview(Id, C) ->
    {ok, Url} = z_media_tag:url(Id, [{width,37}], C),
    [_, Path] = binary:split(Url, <<"/image/">>),
    {ok, Info} = z_file_request:lookup_file(z_url:url_decode(Path), C),
    {ok, Local} = z_file_request:content_file(Info, C),
    Local.
context() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = m_filestore:install(install, C),
    ok = z_notifier:observe(filestore, {?MODULE, observe_filestore, []}, self(), 10, C),
    C.

%% Exercise backend deletion without enabling background uploads in these tests.
observe_filestore(#filestore{action=delete_status}=Request, Context) ->
    mod_filestore:observe_filestore(Request, Context);
observe_filestore(_, _) -> undefined.
anonymous() -> z_context:new(zotonic_site_testsandbox).
unique() -> z_ids:id(24).
digest(Bytes) -> binary:encode_hex(crypto:hash(sha256, Bytes), lowercase).
store(Path, C) -> m_filestore:store(Path, 1, <<"s3">>, <<"https://example.test/", Path/binary>>, false, C).
