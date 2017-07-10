-module(oauth_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

oauth_request_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_context:new(zotonic_site_testsandbox),
    ok = z_module_manager:activate_await(mod_oauth, Context),
    ok = z_module_manager:upgrade_await(Context),

    % 1. Drop all OAuth apps
    ok = m_oauth_app:delete_consumers(Context),

    % 2. Make new consumer
    ConsumerProps = m_oauth_app:create_consumer(<<"OAuth Test">>, Context),
    {id, ConsumerId} = proplists:lookup(id, ConsumerProps),

    ok = m_oauth_perms:set(ConsumerId, [ <<"oauth/test">> ], Context),

    % 3a. Make anonymous app token
    {ok, AnonToken} = m_oauth_app:ensure_anonymous_token(ConsumerId, Context),

    % 3b. Should stay the same
    {ok, AnonToken} = m_oauth_app:ensure_anonymous_token(ConsumerId, Context),

    AnonTokenKeys = lists:sort(proplists:get_keys(AnonToken)),
    ?assertEqual([consumer_key, consumer_secret], AnonTokenKeys),

    % 4. Make an non-anonymous App (with access tokens) for the admin user
    {ok, UserToken} = m_oauth_app:create_app(ConsumerId, 1, Context),

    UserTokenKeys = lists:sort(proplists:get_keys(UserToken)),
    ?assertEqual([consumer_key, consumer_secret, token, token_secret], UserTokenKeys),

    % 5. Make a GET request with the anonymous tokens
    Url = z_convert:to_list(
            z_context:abs_url(
                z_dispatcher:url_for(
                    api,
                    [ {api_module, oauth}, {api_method, test} ],
                    Context),
                Context)),

    Consumer = {
        z_convert:to_list(proplists:get_value(consumer_key, AnonToken)),
        z_convert:to_list(proplists:get_value(consumer_secret, AnonToken)),
        hmac_sha1
    },

    AnonGet = oauth:get(
                    Url,
                    [{"b", "2"}, {"a","1"}],
                    Consumer),


        {ok, {{_, 200, _}, _AnonGetHs, AnonGetBody}} = AnonGet,
        ?assertEqual(#{<<"user">> => <<"anon">>}, z_json:decode(list_to_binary(AnonGetBody))),

    % 6. Make a GET request with the user tokens
    UserGet = oauth:get(
                    Url,
                    [{"b", "2"}, {"a","1"}],
                    Consumer,
                    z_convert:to_list(proplists:get_value(token, UserToken)),
                    z_convert:to_list(proplists:get_value(token_secret, UserToken))),

        {ok, {{_, 200, _}, _UserGetHs, UserGetBody}} = UserGet,
        ?assertEqual(#{<<"user">> => <<"auth">>}, z_json:decode(list_to_binary(UserGetBody))),

    % 7. Make a POST request with the user tokens
    UserPost = oauth:post(
                    Url,
                    [{"b", "2"}, {"a","1"}],
                    Consumer,
                    z_convert:to_list(proplists:get_value(token, UserToken)),
                    z_convert:to_list(proplists:get_value(token_secret, UserToken))),

        {ok, {{_, 200, _}, _UserPostHs, UserPostBody}} = UserPost,
        ?assertEqual(#{<<"user">> => <<"auth">>}, z_json:decode(list_to_binary(UserPostBody))).
