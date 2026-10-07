%% @doc Regression coverage for repeatedly deleting and restoring media.
-module(m_backup_media_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

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
        {ok, Original} = file:read_file(File),
        {ok, Id} = m_media:insert_file(#upload{ filename = <<"koe.jpg">>, data = Original }, Context),
        FilestorePid = whereis(z_utils:name_for_site(mod_filestore, Context)),
        ?assert(is_pid(FilestorePid)),
        lists:foreach(fun(_) ->
            offload_media(Id, Context),
            redelete_media(Id, Context),
            #{ <<"filename">> := Restored } = m_media:get(Id, Context),
            ?assertEqual({ok, Original}, file:read_file(z_media_archive:abspath(Restored, Context))),
            ?assertEqual(FilestorePid, whereis(z_utils:name_for_site(mod_filestore, Context)))
        end, lists:seq(1, 2)),
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
    Path = <<"archive/", Filename/binary>>,
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
    ?assertNot(filelib:is_regular(z_media_archive:abspath(Filename, Context))),
    {ok, #{ location := Location }} = m_filestore:lookup(Path, Context),
    % Evict the uploaded copy, forcing recovery through the download stream.
    % Terminate the temporary file entry synchronously: its graceful stop uses
    % an inactivity timeout that refreshes and cache events can interrupt.
    case z_file_entry:where(Filename, Context) of
        undefined -> ok;
        FilePid ->
            case supervisor:terminate_child(z_file_sup, FilePid) of
                ok -> ok;
                {error, not_found} -> ok
            end
    end,
    ok = filezcache:delete(Location).
