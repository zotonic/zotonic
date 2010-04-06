-module(testsandbox_rsc_update_tests).

-include_lib("eunit/include/eunit.hrl").



rsc_update_test() ->
    C = z_acl:sudo(z_context:new(testsandbox)),

    F = fun({rsc_update_done, _, Id, _, _}, Ctx) ->
                ?debugMsg("event!"),
                ?debugFmt("id: ~p", [Id]),
                ?debugFmt("cat: ~p", [m_rsc:p(Id, category_id, Ctx)]),
                ?debugFmt("is_a: ~p", [m_rsc:is_a(Id, Ctx)]),
                ok
        end,

    z_notifier:observe(rsc_update_done, F, C),

    {ok, Id} = m_rsc:insert([{title, <<"Foo">>}, {category, text}], C),

    ?debugMsg("Fooo"),
    ?debugFmt("modified: ~p", [m_rsc:p(Id, modified, C)]),

    z_notifier:detach(rsc_update_done, F, C),
    ok.


rsc_update_async_test() ->
    C = z_acl:sudo(z_context:new(testsandbox)),

    Pid = self(),

    F = fun() ->
                receive 
                    {'$gen_cast',{{rsc_update_done,_, Id, _, _}, Ctx}} ->
                        ?debugFmt("  xxx id: ~p", [Id]),
                        z_depcache:flush(Id, Ctx),
                        ?debugFmt("cat: ~p", [m_rsc:p(Id, category_id, Ctx)]),
                        ?debugFmt("is_a: ~p", [m_rsc:is_a(Id, Ctx)]),
                        Pid ! ok
                end
        end,

    z_notifier:observe(rsc_update_done, spawn(F), C),

    {ok, Id} = m_rsc:insert([{title, <<"Foo">>}, {category, text}], C),

    timer:sleep(1000),
    ?debugMsg("Fooo"),
    ?debugFmt("modified: ~p", [m_rsc:p(Id, modified, C)]),

    z_notifier:observe(rsc_update_done, spawn(F), C),

    m_rsc:update(Id, [{category, image}], C),
    timer:sleep(1000),

    receive
        ok -> ok
    end,
    receive
        ok -> ok
    end,
    ok.
