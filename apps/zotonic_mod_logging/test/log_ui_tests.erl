-module(log_ui_tests).

-include_lib("eunit/include/eunit.hrl").

ui_log_ringbuffer_dedup_test() ->
    {timeout, 20,
     fun() ->
        ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
        Context = z_context:new(zotonic_site_testsandbox),
        ?assertEqual(true, z_module_manager:active(mod_logging, Context)),

        Prefix = z_convert:to_binary(erlang:unique_integer([positive])),
        Message1 = <<"ui-log-test-", Prefix/binary, " duplicate">>,
        Message2 = <<"ui-log-test-", Prefix/binary, " duplicate">>,
        Message3 = <<"ui-log-test-", Prefix/binary, " distinct">>,

        Event1 = event(Message1, 10, <<"Mozilla/5.0 (log-ui-test-a)">>, <<"https://testsandbox.local/test?id=123">>),
        Event2 = event(Message2, 10, <<"Mozilla/5.0 (log-ui-test-b)">>, <<"https://testsandbox.local/test?id=999">>),
        Event3 = event(Message3, 11, <<"Mozilla/5.0 (log-ui-test-c)">>, <<"https://testsandbox.local/test?id=555">>),

        ok = post_event(Event1, Context),

        ?assertEqual(1, mod_logging:observe_tick_1s(tick_1s, Context)),
        Rows1 = matching_rows(Prefix, Context),
        ?assertEqual(1, length(Rows1)),
        ?assertEqual([10], lists:sort([ line(Row) || Row <- Rows1 ])),

        ok = post_event(Event2, Context),
        ok = post_event(Event3, Context),

        ?assertEqual(1, mod_logging:observe_tick_1s(tick_1s, Context)),
        Rows2 = matching_rows(Prefix, Context),
        ?assertEqual(2, length(Rows2)),
        ?assertEqual([10, 11], lists:sort([ line(Row) || Row <- Rows2 ])),
        ok
    end}.

event(Message, Line, UserAgent, Url) ->
    #{
        <<"type">> => <<"error">>,
        <<"message">> => Message,
        <<"file">> => <<"/js/app.js">>,
        <<"line">> => Line,
        <<"col">> => 5,
        <<"stack">> => <<"Error: duplicate test 123\n at fn (app.js:10:5)">>,
        <<"url">> => Url,
        <<"user_agent">> => UserAgent
    }.

post_event(Event, Context) ->
    Url = z_context:abs_url(z_dispatcher:url_for(jslog, Context), Context),
    case z_fetch:fetch(post, Url, Event, [{content_type, <<"application/json">>}, insecure], Context) of
        {ok, {_FinalUrl, _Headers, _Length, _Body}} -> ok;
        {error, Reason} -> erlang:error({post_failed, Reason})
    end.

matching_rows(Prefix, Context) ->
    {ok, Rows} = z_db:qmap_props(
        "select * from log_ui order by id desc limit 50",
        [],
        [{keys, binary}],
        Context),
    [ Row || Row <- Rows, has_prefix(maps:get(<<"message">>, Row, <<>>), Prefix) ].

has_prefix(Message, Prefix) when is_binary(Message), is_binary(Prefix) ->
    binary:match(Message, <<"ui-log-test-", Prefix/binary>>) =/= nomatch.

line(Row) ->
    maps:get(<<"line">>, Row).
