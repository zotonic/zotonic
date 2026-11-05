%% @doc Regression coverage for repeatedly deleting and restoring media.
-module(m_backup_media_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

-export([cache_eviction_race/0]).

redelete_media_test_() ->
    {timeout, 60, fun redelete_media/0}.

redelete_media() ->
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = z_module_manager:activate_await(mod_backup, Context),
    File = filename:join(code:priv_dir(zotonic_site_testsandbox), "files/archive/koe.jpg"),
    {ok, Data} = file:read_file(File),
    {ok, Id} = m_media:insert_file(#upload{ filename = <<"koe.jpg">>, data = Data }, Context),
    redelete_media(Id, Context),
    % Older recoveries left this column NULL; raw reads supply its default.
    {ok, 1} = z_db:update(rsc, Id, #{ <<"content_group_id">> => undefined }, Context),
    z_depcache:flush(Id, Context),
    redelete_media(Id, Context).

redelete_media(Id, Context) ->
    lists:foreach(
        fun(_) ->
            ?assert(is_map(m_media:get(Id, Context))),
            ?assertEqual(ok, m_rsc:delete(Id, Context)),
            ?assertNot(m_rsc:exists(Id, Context)),
            ?assert(m_rsc_gone:is_gone(Id, Context)),
            #search_result{ result = Deleted } = m_backup_revision:list_deleted({1, 100}, Context),
            ?assert(lists:any(fun(#{ <<"id">> := GoneId }) -> GoneId =:= Id end, Deleted)),
            Rev = z_db:q1("select id from backup_revision where rsc_id = $1
                order by created desc, id desc limit 1", [Id], Context),
            ?assertEqual(ok, m_backup_revision:revert_resource(Id, Rev, [], Context)),
            ?assert(m_rsc:exists(Id, Context)),
            ?assertEqual(m_rsc:p(Id, content_group_id, Context),
                z_db:q1("select content_group_id from rsc where id = $1", [Id], Context)),
            ?assertNot(m_rsc_gone:is_gone(Id, Context))
        end,
        lists:seq(1, 3)).

%% Keep the real filestore database, uploader, cache and media recovery. Only
%% the remote S3 transport is replaced, so no external account is needed.
filestore_redelete_test_() ->
    {timeout, 60, fun filestore_redelete/0}.

filestore_redelete() ->
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = z_module_manager:activate_await(mod_backup, Context),
    ok = z_module_manager:activate_await(mod_filestore, Context),
    Remote = ets:new(remote_media, [public, set]),
    ok = meck:new(filestore_config, [passthrough, no_link]),
    ok = meck:new(s3filez, [passthrough, no_link]),
    try
        lists:foreach(fun({Key, Value}) ->
            meck:expect(filestore_config, Key, fun(_) -> Value end)
        end, [{service, <<"s3">>}, {s3url, <<"https://media.example.test">>},
              {s3key, <<"test">>}, {s3secret, <<"test">>}, {tls_options, []},
              {is_local_keep, false}, {is_upload_enabled, false},
              {delete_interval, <<"false">>}]),
        ok = meck:expect(s3filez, put, fun(_, Location, {filename, _, Path}, _) ->
            {ok, Data} = file:read_file(Path),
            true = ets:insert(Remote, {Location, Data}),
            ok
        end),
        ok = meck:expect(s3filez, stream, fun(_, Location, Callback) ->
            [{Location, Data}] = ets:lookup(Remote, Location),
            Callback(stream_start),
            Callback(Data),
            Callback(eof),
            ok
        end),
        File = filename:join(code:priv_dir(zotonic_site_testsandbox), "files/archive/koe.jpg"),
        {ok, Image} = file:read_file(File),
        %% A distinct JPEG comment gives this fixture its own CAFS object.
        %% Reusing koe.jpg shares bytes and background cache readers with other
        %% resources, so their downloads can race with the recovery assertion.
        <<16#ff, 16#d8, ImageRest/binary>> = Image,
        Comment = z_ids:id(),
        CommentSize = byte_size(Comment) + 2,
        Original = <<16#ff, 16#d8, 16#ff, 16#fe, CommentSize:16,
            Comment/binary, ImageRest/binary>>,
        {ok, Id} = m_media:insert_file(#upload{ filename = <<"koe.jpg">>, data = Original }, Context),
        FilestorePid = whereis(z_utils:name_for_site(mod_filestore, Context)),
        ?assert(is_pid(FilestorePid)),
        lists:foreach(fun(_) ->
            Location = offload_media(Id, Context),
            StreamArgs = ['_', Location, '_'],
            Reads = meck:num_calls(s3filez, stream, StreamArgs),
            redelete_media(Id, Context),
            ?assertEqual(Reads, meck:num_calls(s3filez, stream, StreamArgs)),
            #{ <<"filename">> := Restored } = m_media:get(Id, Context),
            {ok, RestoredHash} = m_cafs:lookup(Restored, Context),
            ?assertEqual({ok, Original}, m_cafs:with_local_file(RestoredHash, fun file:read_file/1, Context)),
            ?assert(meck:num_calls(s3filez, stream, StreamArgs) > Reads),
            ?assertEqual(FilestorePid, whereis(z_utils:name_for_site(mod_filestore, Context)))
        end, lists:seq(1, 2)),
        ?assertEqual(1, ets:info(Remote, size)),
        ?assert(meck:num_calls(s3filez, stream, '_') > 0),
        ?assert(meck:validate(s3filez))
    after
        meck:unload(s3filez),
        meck:unload(filestore_config),
        ets:delete(Remote),
        z_module_manager:deactivate(mod_filestore, Context)
    end.

offload_media(Id, Context) ->
    #{ <<"filename">> := Filename } = Medium = m_media:get(Id, Context),
    {ok, Hash} = m_cafs:lookup(Filename, Context),
    Path = m_cafs:path(Hash),
    % Synchronize queueing with the test; duplicate queue entries are harmless.
    _ = mod_filestore:observe_media_update_done(
        #media_update_done{
            action = insert,
            id = Id,
            pre_is_a = [],
            post_is_a = m_rsc:is_a(Id, Context),
            post_props = Medium
        }, Context),
    QueueId = z_db:q1("select id from filestore_queue where path = $1", [Path], Context),
    {Pid, Ref} = spawn_monitor(fun() ->
        filestore_uploader:upload_job(QueueId, Path, {error, enoent}, Medium, Context)
    end),
    receive
        {'DOWN', Ref, process, Pid, Reason} -> ?assertEqual(normal, Reason)
    after 10000 -> error(upload_timeout)
    end,
    ?assertNot(filelib:is_regular(z_path:files_subdir(Path, Context))),
    {ok, #{ location := Location }} = m_filestore:lookup(Path, Context),
    % Evict the uploaded copy, forcing recovery through the download stream.
    % Terminate the temporary file entry synchronously: its graceful stop uses
    % an inactivity timeout that refreshes and cache events can interrupt.
    lists:foreach(fun(FilePid) ->
        supervisor:terminate_child(z_file_sup, FilePid)
    end, m_cafs:readers(Path, Context)),
    case z_file_entry:where(Filename, Context) of
        undefined -> ok;
        FilePid ->
            case supervisor:terminate_child(z_file_sup, FilePid) of
                ok -> ok;
                {error, not_found} -> ok
            end
    end,
    ok = delete_cached_media(Location, 3),
    Location.

%% A cache entry can stop normally after being logged, between lookup and the
%% delete call, or still be busy releasing readers. Retry the public API so it
%% also removes the persisted entry; return the last error if retries run out.
delete_cached_media(Location, Retries) ->
    try filezcache:delete(Location) of
        {error, _Reason} when Retries > 0 ->
            timer:sleep(20),
            delete_cached_media(Location, Retries - 1);
        Result -> Result
    catch
        exit:{normal, {gen_statem, call, [_Pid, delete, infinity]}} when Retries > 0 ->
            delete_cached_media(Location, Retries - 1)
    end.

%% Keep the cache mock out of the integration test site's VM.
cache_eviction_race_test_() ->
    {timeout, 30, fun() ->
        {ok, Peer, _} = peer:start_link(#{connection => standard_io,
            args => ["-pa" | code:get_path()]}),
        try
            ok = peer:call(Peer, ?MODULE, cache_eviction_race, [], 30000)
        after
            peer:stop(Peer)
        end
    end}.

cache_eviction_race() ->
    ok = meck:new(filezcache, [no_link]),
    Calls = atomics:new(1, []),
    Location = <<"https://media.example.test/cache-eviction-race">>,
    NormalExit = {normal, {gen_statem, call, [self(), delete, infinity]}},
    try
        meck:expect(filezcache, delete, fun(_) ->
            case atomics:add_get(Calls, 1, 1) of
                1 -> exit(NormalExit);
                2 -> ok
            end
        end),
        ?assertEqual(ok, delete_cached_media(Location, 3)),
        ?assertEqual(2, atomics:get(Calls, 1)),
        meck:expect(filezcache, delete, fun(_) -> {error, active} end),
        ?assertEqual({error, active}, delete_cached_media(Location, 3)),
        meck:expect(filezcache, delete, fun(_) -> exit(unexpected_failure) end),
        ?assertExit(unexpected_failure, delete_cached_media(Location, 3)),
        atomics:put(Calls, 1, 0),
        meck:expect(filezcache, delete, fun(_) ->
            atomics:add(Calls, 1, 1),
            exit(NormalExit)
        end),
        ?assertExit(NormalExit, delete_cached_media(Location, 3)),
        ?assertEqual(4, atomics:get(Calls, 1))
    after
        meck:unload(filezcache)
    end,
    ok.

media_preflight_test_() ->
    {timeout, 60, fun media_preflight/0}.

media_preflight() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = z_module_manager:activate_await(mod_backup, C),
    ok = m_filestore:install(install, C),
    ok = z_notifier:observe(filestore, {mod_filestore, observe_filestore, []}, self(), 1, C),
    Data = <<"preflight-", (z_ids:id())/binary>>,
    Hash = z_crypto:hex_sha2(Data),
    Physical = z_path:files_subdir(m_cafs:path(Hash), C),
    try
        Result = z_db:transaction(fun(Ctx) ->
            {ok, Id} = m_media:insert_file(#upload{filename = <<"preflight.txt">>, data = Data}, Ctx),
            Medium = m_media:get(Id, Ctx),
            %% Notifications are deferred until commit; seed the log inside
            %% this rollback-only fixture so pre-flight can see it.
            ok = m_backup_revision:medium_insert(Id, Medium, Ctx),
            ok = m_backup_revision:save_revision(Id, m_rsc:get(Id, Ctx), Ctx),
            Rev = z_db:q1("select id from backup_revision where rsc_id=$1 order by id desc limit 1", [Id], Ctx),
            Check = fun() -> maps:get(<<"media">>, m_backup_revision:restore_options(Rev, Ctx)) end,
            ?assertEqual(#{original => local, preview => none}, Check()),
            ?assertEqual(undefined, m_backup_revision:restore_options(Rev, z_acl:anondo(Ctx))),
            Poster = <<"preflight/", (z_ids:id())/binary, ".jpg">>,
            ok = m_backup_revision:medium_update(Id, Medium#{<<"preview_filename">> => Poster}, Ctx),
            ?assertEqual(#{original => local, preview => missing}, Check()),
            {Html, _} = z_template:render_to_iolist("_backup_restore_media_check.tpl", [{media, Check()}], Ctx),
            ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Html), <<"saved preview image">>)),
            %% Remote checks inspect registry metadata without starting a download.
            ok = file:delete(Physical),
            {ok, _} = m_filestore:store(m_cafs:path(Hash), byte_size(Data), <<"s3">>,
                <<"https://example.invalid/preflight/", Hash/binary>>, false, Ctx),
            ?assertEqual(#{original => remote, preview => missing}, Check()),
            z_db:q("update filestore set error='timeout' where path=$1", [m_cafs:path(Hash)], Ctx),
            ?assertEqual(#{original => unknown, preview => missing}, Check()),
            z_db:q("update filestore set is_deleted=true where path=$1", [m_cafs:path(Hash)], Ctx),
            ?assertEqual(#{original => missing, preview => missing}, Check()),
            %% Legacy revisions without CAFS mappings use their original path.
            Legacy = z_media_archive:abspath(maps:get(<<"filename">>, Medium), Ctx),
            z_db:q("delete from filename_cafs where filename=$1", [maps:get(<<"filename">>, Medium)], Ctx),
            ok = z_filelib:ensure_dir(Legacy),
            ok = file:write_file(Legacy, Data),
            try ?assertEqual(#{original => local, preview => missing}, Check())
            after file:delete(Legacy) end,
            throw({rollback, checked})
        end, C),
        ?assertEqual({rollback, checked}, Result)
    after
        z_notifier:detach(filestore, self(), C),
        file:delete(Physical)
    end.
