%% @doc Preview retirement tests against the disposable testsandbox database.
-module(z_cafs_preview_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

edited_preview_test_() -> {timeout,120,fun edited_preview/0}.
edited_preview() ->
    C = context(),
    ok = z_notifier:observe(media_preview_revision,
        {mod_image_edit, observe_media_preview_revision}, 500, C),
    ok = z_notifier:observe(media_preview_options,
        {mod_image_edit, observe_media_preview_options}, 500, C),
    File = filename:join(code:priv_dir(zotonic_site_testsandbox), "files/archive/koe.jpg"),
    {ok, Data} = file:read_file(File),
    {ok, Id} = m_media:insert_file(#upload{filename= <<"koe.jpg">>,data=Data}, C),
    {ok, CopyId} = m_rsc:duplicate(Id, #{}, C),
    try
        #{<<"digest">> := Hash} = m_media:get(Id,C),
        Before = derivative_count(Hash,C),
        Path = image_path(Id, [{width,43}], C),
        CopyPath = image_path(CopyId, [{width,43}], C),
        Original = image_path(Id, [{width,43},{original,true}], C),
        [Local] = lists:usort([preview(P,C) || P <- [Path,CopyPath,Original]]),
        ?assert(filelib:is_regular(Local)),
        ?assertEqual(Before+1, derivative_count(Hash,C)),
        {ok, Id} = m_image_edit:save_settings(Id, #{<<"rotate">> => 90}, C),
        Edited = image_path(Id,[{width,43}],C),
        ?assertNotEqual(Path,Edited),
        _ = preview(Edited,C),
        ?assertEqual(Before+2, derivative_count(Hash,C)),
        ok = z_cafs_preview:maintenance(C),
        ?assertNotEqual(undefined, obsolete(Path,C)),
        ?assertEqual(undefined, obsolete(Original,C)),
        %% A revert revives the old usage without producing another derivative.
        {ok,Id} = m_image_edit:save_settings(Id,undefined,C),
        ok = z_cafs_preview:maintenance(C),
        ?assertEqual(undefined, obsolete(Path,C)),
        {ok,Id} = m_image_edit:save_settings(Id,#{<<"rotate">> => 90},C),
        ok = z_cafs_preview:maintenance(C),
        age_usage(Path,C), age_derivatives(Hash,C),
        ok = z_cafs_preview:maintenance(C),
        ?assert(z_cafs_preview:expired(Path,C)),
        ?assertMatch({error,_},z_file_request:lookup_file(Path,C)),
        %% Sharing bytes must not bypass the obsolete URL's deadline.
        ?assertEqual(Local,preview(CopyPath,C)),
        ?assertEqual(Local,preview(Original,C)),
        ?assertEqual(Before+2,derivative_count(Hash,C)),
        %% Signed provenance cannot be changed independently of the URL signature.
        Tampered = binary:replace(Path, z_cafs_preview:fingerprint(CopyId,C), binary:copy(<<"0">>,64)),
        ?assertMatch({error,checksum_invalid},z_media_tag:url2props(Tampered,C)),
        ok = m_rsc:delete(CopyId,C),
        ok = m_rsc:delete(Id,C),
        ok = z_cafs_preview:maintenance(C),
        z_db:q("update cafs_preview_usage set obsolete_since=now()-interval '91 days' where source_hash=$1",[Hash],C),
        ok = z_cafs_preview:maintenance(C),
        ?assertEqual(0,derivative_count(Hash,C))
    after
        m_rsc:delete(Id,C), m_rsc:delete(CopyId,C),
        z_notifier:detach(media_preview_revision,C),
        z_notifier:detach(media_preview_options,C)
    end.

mediaclass_retirement_test_() -> {timeout,60,fun mediaclass_retirement/0}.
mediaclass_retirement() ->
    C = context(),
    {Id,Medium,Hash} = source(C),
    Name = maps:get(<<"filename">>,Medium),
    Path = <<Name/binary,"(class-test).jpg">>,
    Filters = [{mediaclass,{<<"cafs-test">>,<<"old">>}},{editrev,<<"original">>}],
    ok = meck:new(z_mediaclass,[passthrough,no_link]),
    try
        ok = meck:expect(z_mediaclass,get,fun(_,_) -> {ok,[],<<"old">>} end),
        {ok,Url} = z_media_tag:url(Name,[{width,43},{mediaclass,<<"cafs-test">>}],C),
        [_,UrlPath] = binary:split(Url,<<"/image/">>),
        SignedPath = z_url:url_decode(UrlPath),
        {ok,Key} = z_cafs_preview:prepare(Path,Name,Filters,Medium,C),
        {ok,Output} = derivative(Hash,Key,C),
        age_derivatives(Hash,C),
        ok = meck:expect(z_mediaclass,get,fun(_,_) -> {ok,[{format,webp}],<<"new">>} end),
        ?assertMatch({ok,_},z_media_tag:url2props(SignedPath,C)),
        ok = z_cafs_preview:maintenance(C),
        ?assertEqual({ok,Output},z_cafs_preview:lookup(Key,C)),
        Since = obsolete(Path,C),
        {ok,Key} = z_cafs_preview:prepare(Path,Name,Filters,Medium,C),
        ?assertEqual(Since,obsolete(Path,C)),
        ok = meck:expect(z_mediaclass,get,fun(_,_) -> {ok,[],<<"old">>} end),
        ok = z_cafs_preview:maintenance(C),
        ?assertEqual(undefined,obsolete(Path,C)),
        ok = meck:expect(z_mediaclass,get,fun(_,_) -> {error,enoent} end),
        ok = z_cafs_preview:maintenance(C),
        age_usage(Path,C),
        %% Pins and active generation claims protect objects during collection.
        ok = m_cafs:pin(Output,<<"preview-test">>,C),
        ok = z_cafs_preview:maintenance(C),
        ?assertEqual(1,derivative_count(Hash,C)),
        ok = m_cafs:unpin(Output,<<"preview-test">>,C),
        z_db:q("update cafs_derivative set claim_until=now()+interval '1 hour' where source_hash=$1",[Hash],C),
        ok = z_cafs_preview:maintenance(C),
        ?assertEqual(1,derivative_count(Hash,C)),
        z_db:q("update cafs_derivative set claim_until=null where source_hash=$1",[Hash],C),
        ok = z_cafs_preview:maintenance(C),
        ?assertEqual(0,derivative_count(Hash,C)),
        ?assertEqual({error,enoent},z_cafs_preview:prepare(Path,Name,Filters,Medium,C)),
        ?assertEqual({error,enoent},derivative(Hash,Key,C))
    after meck:unload(z_mediaclass), m_rsc:delete(Id,C) end.

legacy_retirement_test_() -> {timeout,60,fun legacy_retirement/0}.
legacy_retirement() ->
    C = context(),
    {Id,Medium,Hash} = source(C),
    Name = maps:get(<<"filename">>,Medium),
    Path = <<Name/binary,"(legacy).jpg">>,
    LegacyFile = filename:join(z_path:media_preview(C), Path),
    ok = z_filelib:ensure_dir(LegacyFile),
    ok = file:write_file(LegacyFile, <<"old preview">>),
    Since = z_db:q1("select legacy_since from cafs_preview_state", C),
    try
        z_db:q("update cafs_preview_state set legacy_since=now()-interval '91 days'", C),
        {ok,Key} = z_cafs_preview:prepare(Path,Name,[],Medium,C),
        ?assertEqual(undefined,obsolete(Path,C)),
        {ok,_} = derivative(Hash,Key,C),
        %% Rows written by the previous implementation also gain a baseline.
        z_db:q("update cafs_preview_usage set edit_revision=null where path=$1", [Path], C),
        age_usage(Path,C), age_derivatives(Hash,C),
        ok = z_cafs_preview:maintenance(C),
        ?assertEqual(undefined,obsolete(Path,C)),
        ?assertEqual(1,derivative_count(Hash,C)),
        %% Only a known change or source removal retires an unversioned URL.
        ok = m_rsc:delete(Id,C),
        ok = z_cafs_preview:maintenance(C),
        age_usage(Path,C),
        ok = z_cafs_preview:maintenance(C),
        ?assertEqual(0,derivative_count(Hash,C)),
        ?assertEqual({error,enoent},z_cafs_preview:prepare(Path,Name,[],Medium,C)),
        ?assertNot(filelib:is_regular(LegacyFile)),
        %% Derivatives predating provenance tracking also age out without reads.
        {ok,_} = derivative(Hash,undefined,C),
        ok = z_cafs_preview:maintenance(C),
        ?assertEqual(1,derivative_count(Hash,C)),
        age_derivatives(Hash,C),
        ok = z_cafs_preview:maintenance(C),
        ?assertEqual(0,derivative_count(Hash,C))
    after
        z_db:q("update cafs_preview_state set legacy_since=$1", [Since], C),
        file:delete(LegacyFile), m_rsc:delete(Id,C)
    end.

upgrade_test_() -> {timeout,60,fun upgrade/0}.
upgrade() ->
    C = context(),
    {Id,_,Hash} = source(C),
    try
        {ok,Output} = derivative(Hash,undefined,C),
        Result = z_db:transaction(fun(Ctx) ->
            z_db:q("alter table cafs_derivative drop column created,drop column checked_at",Ctx),
            z_db:flush(Ctx),
            ok = m_cafs:install(Ctx),
            ok = m_cafs:install(Ctx),
            ?assertEqual(Output,z_db:q1("select output_hash from cafs_derivative where source_hash=$1",[Hash],Ctx)),
            ?assertMatch({ok,_},z_db:column(cafs_derivative,created,Ctx)),
            ?assertMatch({ok,_},z_db:column(cafs_derivative,checked_at,Ctx)),
            {rollback,tested}
        end,C),
        ?assertEqual({rollback,tested},Result)
    after z_db:flush(C),m_rsc:delete(Id,C) end.

context() -> z_acl:sudo(z_context:new(zotonic_site_testsandbox)).
source(C) ->
    {ok,Id} = m_media:insert_file(#upload{filename= <<"preview-test.txt">>,
        data= <<"preview test ",(z_ids:id())/binary>>},C),
    #{<<"digest">> := Hash} = Medium = m_media:get(Id,C),
    {Id,Medium,Hash}.
derivative(Hash,Key,C) ->
    m_cafs:derivative(Hash,[{width,43}],1,Key,fun() ->
        File = z_tempfile:new(),
        try
            ok = file:write_file(File,<<"preview output ",Hash/binary>>),
            m_cafs:stage_file(File,C)
        after file:delete(File) end
    end,C).
image_path(Id,Opts,C) ->
    {ok,Url} = z_media_tag:url(Id,Opts,C),
    [_,Path] = binary:split(Url,<<"/image/">>),
    z_url:url_decode(Path).
preview(Path,C) ->
    {ok,Info} = z_file_request:lookup_file(Path,C),
    {ok,Local} = z_file_request:content_file(Info,C),
    Local.
obsolete(Path,C) -> z_db:q1("select obsolete_since from cafs_preview_usage where path=$1",[Path],C).
age_usage(Path,C) -> z_db:q("update cafs_preview_usage set obsolete_since=now()-interval '91 days' where path=$1",[Path],C).
age_derivatives(Hash,C) -> z_db:q("update cafs_derivative set created=now()-interval '91 days' where source_hash=$1",[Hash],C).
derivative_count(Hash,C) -> z_db:q1("select count(*) from cafs_derivative where source_hash=$1",[Hash],C).
