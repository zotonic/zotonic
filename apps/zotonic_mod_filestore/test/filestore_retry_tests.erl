%% @doc Persistent cooldown, recovery, concurrency and admin read health.
-module(filestore_retry_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").

retry_delay_test() ->
    lists:foreach(fun(Count) ->
        Max = min(600, 5 bsl min(Count, 7)),
        lists:foreach(fun(_) ->
            Delay = m_filestore:retry_delay(Count),
            ?assert(Delay >= Max * 4 div 5),
            ?assert(Delay =< Max)
        end, lists:seq(1, 100))
    end, lists:seq(0, 100)).

retry_recovery_test_() ->
    {timeout, 60, fun retry_recovery/0}.

retry_recovery() ->
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = m_filestore:install(install, Context),
    Path = <<"archive/retry-test-", (z_ids:id())/binary>>,
    {ok, Id} = m_filestore:store(Path, 10, <<"s3">>, <<"https://example.test/", Path/binary>>, false, Context),
    try
        {ok, Claim} = m_filestore:claim_download(Id, Context),
        ?assertEqual({error, enoent}, m_filestore:claim_download(Id, Context)),
        ok = m_filestore:complete_download(Id, Claim, {error, forbidden}, Context),
        {ok, Failed} = m_filestore:lookup(Path, Context),
        ?assertEqual(<<"forbidden">>, maps:get(error, Failed)),
        ?assertEqual(1, maps:get(error_count, Failed)),
        ?assertNot(m_filestore:is_download_ok(Failed)),
        ?assertEqual({error, enoent}, m_filestore:claim_download(Id, Context)),
        % A duplicate callback cannot extend the cooldown or count twice.
        ok = m_filestore:complete_download(Id, Claim, {error, forbidden}, Context),
        ?assertEqual({ok, Failed}, m_filestore:lookup(Path, Context)),
        1 = z_db:q("update filestore set retry_after = now() - interval '1 second',
            error_since = now() - interval '25 hours' where id = $1", [Id], Context),
        ?assert(maps:get(prolonged, m_filestore:health(Context)) >= 1),
        {ok, Retry} = m_filestore:claim_download(Id, Context),
        ok = m_filestore:complete_download(Id, Claim, ok, Context),
        {ok, StillFailed} = m_filestore:lookup(Path, Context),
        ?assertEqual(<<"forbidden">>, maps:get(error, StillFailed)),
        ok = m_filestore:complete_download(Id, Retry, ok, Context),
        {ok, Recovered} = m_filestore:lookup(Path, Context),
        ?assertEqual(undefined, maps:get(error, Recovered)),
        ?assertEqual(undefined, maps:get(error_since, Recovered)),
        ?assertEqual(undefined, maps:get(retry_after, Recovered)),
        ?assertEqual(0, maps:get(error_count, Recovered)),
        % Re-running the migration preserves existing state.
        ok = m_filestore:install(14, Context),
        ?assertEqual({ok, Recovered}, m_filestore:lookup(Path, Context))
    after
        z_db:q("delete from filestore where id = $1", [Id], Context)
    end.

health_acl_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    ?assertEqual({error, eacces}, m_filestore:m_get([<<"health">>], undefined, Context)),
    ?assertEqual({error, eacces}, m_filestore:m_get([<<"prolonged_errors">>, 1], undefined, Context)).

health_threshold_test() ->
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    Result = z_db:transaction(fun(Ctx) ->
        z_db:q("delete from filestore_read_stats", Ctx),
        z_db:q("insert into filestore_read_stats values (date_trunc('minute', now()), 25, 5)", Ctx),
        ?assert(maps:get(high_error_rate, m_filestore:health(Ctx))),
        z_db:q("update filestore_read_stats set attempts = 26", Ctx),
        ?assertNot(maps:get(high_error_rate, m_filestore:health(Ctx))),
        z_db:q("update filestore_read_stats set attempts = 4, failures = 4", Ctx),
        ?assertNot(maps:get(high_error_rate, m_filestore:health(Ctx))),
        z_db:q("update filestore_read_stats set attempts = 5, failures = 5,
            minute = now() - interval '16 minutes'", Ctx),
        ?assertEqual(0, maps:get(failures, m_filestore:health(Ctx))),
        {rollback, tested}
    end, Context),
    ?assertEqual({rollback, tested}, Result).

schema_upgrade_test() ->
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    try
        Result = z_db:transaction(fun(Ctx) ->
            % All DDL is rolled back: other test data is preserved.
            [] = z_db:q("drop table filestore", Ctx),
            z_db:flush(Ctx),
            ok = m_filestore:install(install, Ctx),
            Fresh = z_db:columns(filestore, Ctx),
            {ok, Id} = m_filestore:store(<<"legacy">>, 1, <<"s3">>, <<"https://example.test/legacy">>, false, Ctx),
            [] = z_db:q("alter table filestore drop column error_count,
                drop column error_since, drop column retry_after, drop column download_token,
                drop column is_unrecoverable", Ctx),
            1 = z_db:q("update filestore set error = 'forbidden',
                modified = now() - interval '2 days' where id = $1", [Id], Ctx),
            z_db:flush(Ctx),
            ok = m_filestore:install(14, Ctx),
            ?assertEqual(lists:sort(Fresh), lists:sort(z_db:columns(filestore, Ctx))),
            {ok, Entry} = m_filestore:lookup(<<"legacy">>, Ctx),
            ?assertEqual(maps:get(modified, Entry), maps:get(error_since, Entry)),
            ?assertEqual(undefined, maps:get(retry_after, Entry)),
            ?assertMatch({ok, _}, m_filestore:claim_download(Id, Ctx)),
            {rollback, tested}
        end, Context),
        ?assertEqual({rollback, tested}, Result)
    after
        z_db:flush(Context)
    end.

templates_compile_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    Dir = code:priv_dir(zotonic_mod_filestore),
    lists:foreach(fun(Name) ->
        ?assertMatch({ok, _}, template_compiler:compile_file(
            filename:join([Dir, "templates", Name]),
            [{runtime, z_template_compiler_runtime}], Context))
    end, ["_filestore_health.tpl", "_filestore_prolonged_errors.tpl", "_admin_status_alert.tpl", "admin_filestore.tpl"]).

cache_read_recovery_test_() ->
    [{atom_to_list(Mod) ++ " " ++ atom_to_list(Mode) ++ " " ++ binary_to_list(Expected),
      fun() -> cache_read_recovery(Service, Mod, Reason, Mode, Expected) end}
     || {Service, Mod, Reason, Mode, Expected} <- [
        {<<"s3">>, s3filez, forbidden, wrapped, <<"forbidden">>},
        {<<"webdav">>, webdavfilez, forbidden, bare, <<"forbidden">>},
        {<<"webdav">>, webdavfilez, 401, bare, <<"http_401">>},
        {<<"webdav">>, webdavfilez, timeout, returned, <<"timeout">>},
        {<<"ftp">>, ftpfilez, epath, wrapped, <<"epath">>},
        {<<"ftp">>, ftpfilez, elogin, wrapped, <<"elogin">>}
     ]].

cache_read_recovery(Service, Mod, Reason, Mode, Expected) ->
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    Path = <<"archive/cache-retry-", (z_ids:id())/binary>>,
    Location = <<"https://example.test/", Path/binary>>,
    {ok, Id} = m_filestore:store(Path, 3, Service, Location, false, Context),
    Modules = [filezcache, z_notifier, Mod],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    try
        meck:expect(z_notifier, first, fun(#filestore_credentials_revlookup{}, _) ->
            {ok, #filestore_credentials{
                service = Service,
                service_url = <<"https://example.test/">>,
                location = Location,
                credentials = #{}
            }}
        end),
        meck:expect(filezcache, locate_monitor, fun(_) -> {error, enoent} end),
        meck:expect(filezcache, insert_stream, fun(_, _, Fun, _) ->
            % Run synchronously to make callback state transitions observable.
            try Fun(self()) catch exit:_ -> ok end,
            {ok, self()}
        end),
        meck:expect(Mod, stream, fun(_, _, Fun) ->
            case Mode of
                wrapped -> Fun({error, Reason});
                bare -> Fun(Reason);
                returned -> {error, Reason}
            end
        end),
        ?assertMatch({ok, _}, mod_filestore:lookup(Path, Context)),
        ?assertEqual(undefined, mod_filestore:lookup(Path, Context)),
        {ok, FailedEntry} = m_filestore:lookup(Path, Context),
        ?assertEqual(Expected, maps:get(error, FailedEntry)),
        ?assertEqual(1, maps:get(error_count, FailedEntry)),
        ?assertEqual(1, meck:num_calls(Mod, stream, '_')),
        1 = z_db:q("update filestore set retry_after = now() - interval '1 second' where id = $1", [Id], Context),
        meck:expect(Mod, stream, fun(_, _, Fun) ->
            Fun(stream_start), Fun(<<"abc">>), Fun(eof), ok
        end),
        meck:expect(filezcache, append_stream, fun(_, _) -> ok end),
        meck:expect(filezcache, finish_stream, fun(_) -> ok end),
        ?assertMatch({ok, _}, mod_filestore:lookup(Path, Context)),
        {ok, Entry} = m_filestore:lookup(Path, Context),
        ?assertEqual(undefined, maps:get(error, Entry)),
        ?assertEqual(2, meck:num_calls(Mod, stream, '_')),
        1 = z_db:q("update filestore set error = 'timeout',
            error_since = now() - interval '25 hours' where id = $1", [Id], Context),
        ok = m_filestore:mark_unrecoverable(Id, Context),
        ?assertEqual(undefined, mod_filestore:lookup(Path, Context)),
        ?assertEqual(2, meck:num_calls(Mod, stream, '_'))
    after
        lists:foreach(fun meck:unload/1, Modules),
        z_db:q("delete from filestore where id = $1", [Id], Context)
    end.

prolonged_errors_test() ->
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    Result = z_db:transaction(fun(Ctx) ->
        z_db:q("delete from filestore", Ctx),
        54 = z_db:q("insert into filestore (path, service, location, error, error_count, error_since)
            select 'error-' || n, 's3', 'https://example.test/' || n,
                'forbidden', 7, now() - (100 + n) * interval '1 hour'
            from generate_series(1, 54) as n", Ctx),
        1 = z_db:q("update filestore set error = null where path = 'error-54'", Ctx),
        1 = z_db:q("update filestore set is_deleted = true where path = 'error-53'", Ctx),
        1 = z_db:q("update filestore set error_since = now() where path = 'error-52'", Ctx),
        {ok, {First, []}} = m_filestore:m_get([<<"prolonged_errors">>, <<"invalid">>], undefined, Ctx),
        #{entries := Entries, page := 1, previous := undefined, next := 2} = First,
        ?assertEqual(50, length(Entries)),
        [Oldest | _] = Entries,
        ?assertEqual(<<"error-51">>, maps:get(path, Oldest)),
        ?assertEqual(<<"forbidden">>, maps:get(error, Oldest)),
        ?assertEqual(7, maps:get(error_count, Oldest)),
        ?assertNot(maps:is_key(location, Oldest)),
        {ok, {Second, []}} = m_filestore:m_get([<<"prolonged_errors">>, <<"2">>], undefined, Ctx),
        ?assertMatch(#{entries := [#{path := <<"error-1">>}], previous := 1, next := undefined}, Second),
        {rollback, tested}
    end, Context),
    ?assertEqual({rollback, tested}, Result).

unrecoverable_test() ->
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    Result = z_db:transaction(fun(Ctx) ->
        {ok, Id} = m_filestore:store(<<"unrecoverable-test">>, 1, <<"s3">>,
            <<"https://example.test/unrecoverable">>, false, Ctx),
        ?assertEqual({error, eacces}, m_filestore:mark_unrecoverable(Id, Ctx#context{user_id = undefined})),
        % Even an administrator ACL is insufficient: the user must be 1.
        ?assertEqual({error, eacces}, m_filestore:mark_unrecoverable(Id, Ctx#context{user_id = 2})),
        ?assertEqual({error, eacces}, m_filestore:mark_unrecoverable(Id, Ctx#context{acl = undefined, acl_is_read_only = true})),
        ?assertEqual({error, badarg}, m_filestore:mark_unrecoverable(<<"invalid">>, Ctx)),
        ?assertEqual({error, not_eligible}, m_filestore:mark_unrecoverable(Id, Ctx)),
        1 = z_db:q("update filestore set error = 'forbidden', error_since = now() where id = $1", [Id], Ctx),
        ?assertEqual({error, not_eligible}, m_filestore:mark_unrecoverable(Id, Ctx)),
        1 = z_db:q("update filestore set error_since = now() - interval '25 hours' where id = $1", [Id], Ctx),
        {ok, Claim} = m_filestore:claim_download(Id, Ctx),
        Before = maps:get(prolonged, m_filestore:health(Ctx)),
        ok = m_filestore:mark_unrecoverable(Id, Ctx),
        ?assertEqual(Before - 1, maps:get(prolonged, m_filestore:health(Ctx))),
        ?assertEqual({error, enoent}, m_filestore:claim_download(Id, Ctx)),
        % A response to a request already in flight must not undo the decision.
        ok = m_filestore:complete_download(Id, Claim, ok, Ctx),
        {ok, Entry} = m_filestore:lookup(<<"unrecoverable-test">>, Ctx),
        ?assertEqual(true, maps:get(is_unrecoverable, Entry)),
        ?assertEqual(false, maps:get(is_deleted, Entry)),
        ?assertEqual(<<"forbidden">>, maps:get(error, Entry)),
        ?assertEqual({error, not_eligible}, m_filestore:mark_unrecoverable(Id, Ctx)),
        {ok, Id} = m_filestore:store(<<"unrecoverable-test">>, 1, <<"s3">>,
            <<"https://example.test/unrecoverable">>, false, Ctx),
        {ok, Replaced} = m_filestore:lookup(<<"unrecoverable-test">>, Ctx),
        ?assertEqual(false, maps:get(is_unrecoverable, Replaced)),
        {rollback, tested}
    end, Context),
    ?assertEqual({rollback, tested}, Result).
