-module(action_wires_event_tests).
-include_lib("eunit/include/eunit.hrl").

%% Quotes and newlines in generated actions must remain executable JavaScript
%% inside the timer callback, including the continuation alias.
timer_callback_test_() ->
    {setup,
        fun() ->
            meck:new(z_render, [no_link]),
            meck:expect(z_render, make_postback,
                fun(_, _, _, _, _, _, _) -> {<<"postback('target');\n">>, undefined} end),
            meck:expect(z_render, render_actions,
                fun(_, _, _, Context) -> {<<"update(\"target\");\n">>, Context} end)
        end,
        fun(_) -> meck:unload(z_render) end,
        [?_test(begin
            {Script, context} = action_wires_event:render_action(undefined, undefined,
                [{type, Type}, {interval, 5000}], context),
            ?assertEqual(
                <<"setTimeout(function() { postback('target');\nupdate(\"target\");\n }, 5000);\n">>,
                iolist_to_binary(Script))
        end) || Type <- [interval, <<"interval">>, "interval", continuation]]}.
