-module(translation_language_isolation_tests).
-moduledoc("Regression checks for independent document and preview languages.").
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([run/1]).

%% Read-only integration checks; no language configuration or resources are changed.
run(Site) ->
    Context = z_acl:sudo(z_context:new(list_to_existing_atom(Site))),
    eunit:test(fun() -> language_isolation(Context) end, [verbose]).

language_isolation_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    language_isolation(z_acl:sudo(z_context:new(zotonic_site_testsandbox))).

language_isolation(Context) ->
    Default = z_language:default_language(Context),
    Languages = z_language:editable_languages(Context),
    lists:foreach(fun(Language) ->
        lists:foreach(fun(Destination) ->
            Request = request(Destination, Language, Default, Context),
            Result = init(Request),
            ?assertEqual(Language, z_context:language(Result)),
            ?assertEqual(#{}, maps:get(resp_cookies, Result#context.cowreq, #{}))
        end, [<<"iframe">>, <<"frame">>, <<"empty">>]),

        % A preview flag also suppresses the cookie without Fetch Metadata.
        Legacy = z_context:set_q(<<"z_language_persist">>, <<"false">>,
            request(undefined, Language, Default, Context)),
        ?assertEqual(#{}, maps:get(resp_cookies, (init(Legacy))#context.cowreq, #{})),

        % Auth refresh must return the document's language to its MQTT session.
        Auth = mod_translation:observe_request_context(#request_context{
            phase = auth_status,
            document = #{<<"language">> => #{<<"document">> => atom_to_binary(Language)}}
        }, z_context:set_language(Default, Context), Context),
        ?assertEqual(Language, z_context:language(Auth)),
        {ok, Mqtt} = z_mqtt_sessions_runtime:control_message([<<"auth">>],
            #{payload => #{<<"user_id">> => z_acl:user(Context),
                <<"preferences">> => #{<<"language">> => atom_to_binary(Language)}}},
            z_context:set_language(Default, Context)),
        ?assertEqual(Language, z_context:language(Mqtt))
    end, Languages),

    % Normal top-level navigation still sets the cookie.
    Top = init(request(<<"document">>, Default, undefined, Context)),
    ?assert(maps:is_key(<<"z.lang">>, maps:get(resp_cookies, Top#context.cowreq))),
    Socket = request(undefined, Default, undefined, Context),
    SocketResult = init(Socket#context{cowreq = #{headers => #{<<"upgrade">> => <<"websocket">>}}}),
    ?assertEqual(#{}, maps:get(resp_cookies, SocketResult#context.cowreq, #{})).

request(Destination, Language, CookieLanguage, Context) ->
    Headers = case Destination of
        undefined -> #{};
        _ -> #{<<"sec-fetch-dest">> => Destination}
    end,
    Cookies = case CookieLanguage of
        undefined -> [];
        _ -> [{<<"z.lang">>, atom_to_binary(CookieLanguage)}]
    end,
    z_context:set_q(<<"z_language">>, atom_to_binary(Language), Context#context{
        cowreq = #{headers => Headers},
        cowenv = #{cowmachine_cookies => Cookies}
    }).

init(Context) ->
    mod_translation:observe_request_context(#request_context{phase = init}, Context, Context).
