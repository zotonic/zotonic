-module(oauth2_tests).

-include_lib("eunit/include/eunit.hrl").
% -include_lib("zotonic_core/include/zotonic.hrl").

oauth2_request_test() ->
    {timeout, 20,
     fun() ->
        ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
        Context = z_context:new(zotonic_site_testsandbox),
        SudoContext = z_acl:sudo(Context),
        ok = z_module_manager:activate_await(mod_oauth2, Context),
        ok = z_module_manager:upgrade_await(Context),

        App = #{
            <<"is_enabled">> => true,
            <<"description">> => <<"TestApp">>
        },
        {ok, AppId} = m_oauth2:insert_app(App, z_acl:sudo(Context)),

        % Make a new OAuth2 token with full access to the admin (user 1) account
        TPs = #{
            <<"is_read_only">> => false,
            <<"is_full_access">> => true
        },

        % Should have permission to make a token for user 1
        {error, eacces} = m_oauth2:insert_token(AppId, 1, <<"test">>, TPs, Context),

        {ok, TId_1} = m_oauth2:insert_token(AppId, 1, <<"test">>, TPs, SudoContext),
        {ok, Token_1} = m_oauth2:encode_bearer_token(TId_1, 60, SudoContext),
        {ok, #{ <<"id">> := TId_1}} = m_oauth2:decode_bearer_token(Token_1, Context),

        {ok, Token_1a} = m_oauth2:encode_bearer_token(TId_1, undefined, SudoContext),
        {ok, #{ <<"id">> := TId_1 }} = m_oauth2:decode_bearer_token(Token_1a, Context),

        % Tokens can expire
        {ok, Token_1t} = m_oauth2:encode_bearer_token(TId_1, -1, SudoContext),
        {error, expired} = m_oauth2:decode_bearer_token(Token_1t, Context),

        Url = z_context:abs_url( z_dispatcher:url_for(api, [ {star, <<"model/acl/get/user">> } ], Context), Context),

        % No token
        {ok, {_, _, _, NoT}} = z_url_fetch:fetch(Url, [ insecure ]),
        #{ <<"result">> := undefined, <<"status">> := <<"ok">> } = jsxrecord:decode(NoT),

        % Valid tokens
        {ok, {_, _, _, T1}} = z_url_fetch:fetch(Url, [ {authorization, <<"Bearer ", Token_1/binary>>}, insecure ]),
        #{ <<"result">> := 1, <<"status">> := <<"ok">> } = jsxrecord:decode(T1),

        {ok, {_, _, _, T1a}} = z_url_fetch:fetch(Url, [ {authorization, <<"Bearer ", Token_1a/binary>>}, insecure ]),
        #{ <<"result">> := 1, <<"status">> := <<"ok">> } = jsxrecord:decode(T1a),

        % Expired token
        {ok, {_, _, _, T1t}} = z_url_fetch:fetch(Url, [ {authorization, <<"Bearer ", Token_1t/binary>>}, insecure ]),
        #{ <<"result">> := undefined, <<"status">> := <<"ok">> } = jsxrecord:decode(T1t),

        % Illegal token
        {ok, {_, _, _, Tx}} = z_url_fetch:fetch(Url, [ {authorization, <<"Bearer ", Token_1/binary, "xxx">>}, insecure ]),
        #{ <<"result">> := undefined, <<"status">> := <<"ok">> } = jsxrecord:decode(Tx),

        % Uknown token
        {ok, {_, _, _, Tu}} = z_url_fetch:fetch(Url, [ {authorization, <<"Bearer xxx">>}, insecure ]),
        #{ <<"result">> := undefined, <<"status">> := <<"ok">> } = jsxrecord:decode(Tu),

        % TODO:
        % - test limitations on groups
        % - test read only flag
        % - test IP restrictions

        ok
    end}.

