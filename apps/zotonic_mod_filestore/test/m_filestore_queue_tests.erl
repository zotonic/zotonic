%% @doc Concurrent queue writers must preserve one entry without aborting transactions.
-module(m_filestore_queue_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

concurrent_queue_test_() ->
    {timeout, 60, fun concurrent_queue/0}.

concurrent_queue() ->
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = m_filestore:install(install, Context),
    Path = <<"archive/queue-test-", (z_ids:id())/binary>>,
    Parent = self(),
    try
        Workers = [spawn_monitor(fun() ->
            receive go -> ok end,
            Result = z_db:transaction(fun(Ctx) ->
                Queued = m_filestore:queue(Path, #{ <<"id">> => N }, Ctx),
                % A duplicate must leave the caller's transaction usable.
                1 = z_db:q1("select 1", Ctx),
                Queued
            end, Context),
            Parent ! {self(), Result}
        end) || N <- lists:seq(1, 8)],
        lists:foreach(fun({Pid, _}) -> Pid ! go end, Workers),
        Results = [receive
            {Pid, Result} ->
                receive
                    {'DOWN', Ref, process, Pid, Reason} -> ?assertEqual(normal, Reason)
                after 10000 -> error(queue_worker_timeout)
                end,
                Result;
            {'DOWN', Ref, process, Pid, Reason} -> error({queue_worker_failed, Reason})
        after 10000 -> error(queue_timeout)
        end || {Pid, Ref} <- Workers],
        ?assertEqual([ok | lists:duplicate(7, {error, duplicate})], lists:sort(Results)),
        ?assertEqual(1, z_db:q1("select count(*) from filestore_queue where path = $1", [Path], Context)),
        Props = z_db:q1("select props from filestore_queue where path = $1", [Path], Context),
        ?assertEqual({error, duplicate}, m_filestore:queue(Path, #{ <<"id">> => 99 }, Context)),
        ?assertEqual(Props, z_db:q1("select props from filestore_queue where path = $1", [Path], Context))
    after
        z_db:q("delete from filestore_queue where path = $1", [Path], Context)
    end.
