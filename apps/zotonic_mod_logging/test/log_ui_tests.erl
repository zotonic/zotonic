-module(log_ui_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

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

        ?assertEqual(ok, mod_logging:drain_ui_log(Context)),
        Rows1 = matching_rows(Prefix, Context),
        ?assertEqual(1, length(Rows1)),
        ?assertEqual([10], lists:sort([ line(Row) || Row <- Rows1 ])),

        ok = post_event(Event2, Context),
        ok = post_event(Event3, Context),

        ?assertEqual(ok, mod_logging:drain_ui_log(Context)),
        Rows2 = matching_rows(Prefix, Context),
        ?assertEqual(2, length(Rows2)),
        ?assertEqual([10, 11], lists:sort([ line(Row) || Row <- Rows2 ])),
        ok
    end}.

csp_report_sample_test() ->
    {timeout, 20,
     fun() ->
        ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
        Context = z_context:new(zotonic_site_testsandbox),
        ?assertEqual(true, z_module_manager:active(mod_logging, Context)),
        ok = mod_logging:clear_csp_reports(Context),

        {ok, Pid} = z_module_manager:whereis(mod_logging, Context),
        Prefix = z_convert:to_binary(erlang:unique_integer([positive])),
        BlockedUrl = <<"https://blocked.example/", Prefix/binary>>,
        DocumentUrl = z_context:abs_url(<<"/csp-sample-test">>, Context),
        Sample = <<"<script>alert('sample')</script>">>,
        Report = #{
            <<"blockedURL">> => BlockedUrl,
            <<"effectiveDirective">> => <<"script-src-elem">>,
            <<"originalPolicy">> => <<"script-src 'self' 'report-sample'">>,
            <<"documentURL">> => DocumentUrl,
            <<"sourceFile">> => DocumentUrl,
            <<"lineNumber">> => 10,
            <<"columnNumber">> => 20,
            <<"sample">> => Sample
        },
        Notification = #content_security_report{
            type = <<"csp-violation">>,
            url = DocumentUrl,
            body = Report,
            user_agent = <<"CSP sample test">>
        },
        ?assertEqual(ok,
            mod_logging:pid_observe_content_security_report(Pid, Notification, Context)),
        ?assertEqual(ok,
            mod_logging:pid_observe_content_security_report(
                Pid,
                Notification#content_security_report{body = maps:remove(<<"sample">>, Report)},
                Context)),

        {ok, Reports} = mod_logging:csp_reports(Context),
        [Stored] = [ R || R <- Reports, maps:get(blocked_url, R) =:= BlockedUrl ],
        ?assertEqual(2, maps:get(count, Stored)),
        ?assertEqual([ Sample ], maps:get(samples, Stored))
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
