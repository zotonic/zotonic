%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2009-10-02
%% @doc OAuth; application (server) store.

%% Copyright 2009 Arjan Scherpenisse
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(m_oauth_app).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-behaviour(gen_model).


-include_lib("zotonic_core/include/zotonic.hrl").


-export([
         m_find_value/3,
         m_to_list/2,
         m_value/2,

         consumer_tokens/2,
         consumer_access_tokens/2,
         delete_consumer_token/2,

         consumer_lookup/2,
         secrets_for_verify/4,
         check_nonce/5,
         request_token/2,
         authorize_request_token/3,
         exchange_request_for_access/2,

         ensure_anonymous_token/2,

         get_request_token/2,

         create_app/2,
         create_app/3,

         get_app_tokens/2,

         create_consumer/2,
         create_consumer/5,
         update_consumer/3,
         get_consumer/2,
         delete_consumer/2,
         delete_consumers/1,
         reset_consumer_secret/2
         ]).

-define(TS_SKEW, 600).


%% gen_model

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(info, #m{value=undefined} = M, _Context) ->
    M#m{value=info};
m_find_value(Id, #m{value=info}, Context) ->
    get_consumer(Id, Context);

m_find_value(tokens, #m{value=undefined} = M, _Context) ->
    M#m{value=tokens};
m_find_value(Id, #m{value=tokens}, Context) ->
    consumer_tokens(Id, Context);
m_find_value(Id, #m{value=access_tokens}, Context) ->
    consumer_access_tokens(Id, Context).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> list()
m_to_list(#m{value=undefined}, Context) ->
    all_apps(Context).

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=_}, _Context) ->
    undefined.


all_apps(Context) ->
    z_db:assoc_props("
            SELECT *
            FROM oauth_application_registry
            WHERE user_id = $1
               OR user_id is null
            ORDER BY application_title, id",
            [Context#context.user_id],
            Context).

consumer_tokens(Id, Context) ->
    z_db:assoc("
        SELECT *
        FROM oauth_application_token
        WHERE application_id = $1
        ORDER BY timestamp DESC",
        [Id],
        Context).

consumer_access_tokens(Id, Context) ->
    Tks = z_db:assoc("
        SELECT *
        FROM oauth_application_token
        WHERE application_id = $1
          AND token_type = 'access'
        ORDER BY timestamp DESC",
        [Id],
        Context),
    {Anon, Other} = lists:partition(
        fun(Token) ->
            z_utils:is_empty(proplists:get_value(token_secret, Token))
        end,
        Tks),
    Anon ++ Other.

delete_consumer_token(Id, Context) ->
    case z_db:q("
        delete from oauth_application_token
        where id = $1",
        [Id],
        Context)
    of
        1 -> ok;
        0 -> {error, enoent}
    end.

%%
%% Lookup a application in the application registry.
%%
%% Return a tuple: {Key, Secret, Signature method}
consumer_lookup(Key, Context) ->
    z_db:assoc_row("
        SELECT id, consumer_key, consumer_secret, callback_uri
        FROM oauth_application_registry
        WHERE consumer_key = $1
          AND enabled = true",
        [Key],
        Context).


secrets_for_verify(none, Consumer, _Token, Context) ->
    z_db:assoc_row("
        SELECT id, consumer_key, consumer_secret, callback_uri
        FROM oauth_application_registry
        WHERE consumer_key	= $1
          AND enabled = true",
        [proplists:get_value(consumer_key, Consumer)],
        Context);


secrets_for_verify(access, Consumer, undefined, Context) ->
    % Anonymous access with only consumer key and no token
    z_db:assoc_row("
        SELECT tok.id AS id,
           application_id,
           NULL as user_id,
           consumer_key,
           consumer_secret,
           '' as token,
           '' as token_secret,
           '' as callback_uri
        FROM oauth_application_registry app
           JOIN oauth_application_token tok
           ON app.id = tok.application_id
        WHERE token_type = 'access'
          AND consumer_key = $1
          AND token like '- - %'
          AND enabled = true
          AND token_ttl >= NOW()",
        [proplists:get_value(consumer_key, Consumer)],
        Context);
secrets_for_verify(Type, Consumer, Token, Context) ->
    z_db:assoc_props_row("
        SELECT tok.id AS id,
           application_id,
           tok.user_id as user_id,
           consumer_key,
           consumer_secret,
           token,
           token_secret,
           tok.callback_uri
        FROM oauth_application_registry app
           JOIN oauth_application_token tok
           ON app.id = tok.application_id
        WHERE token_type = $1
          AND consumer_key = $2
          AND token	= $3
          AND enabled = true
          AND token_ttl >= NOW()",
        [Type, proplists:get_value(consumer_key, Consumer), Token],
        Context).


check_nonce(Consumer, Token, Timestamp, Nonce, Context) ->
    CKey = proplists:get_value(consumer_key, Consumer),
    TK = proplists:get_value(token, Token),
    TS = z_convert:to_integer(Timestamp),
    z_db:transaction(
        fun (C) -> check_nonce1(CKey, TK, TS, Nonce, C) end,
        Context).


check_nonce1(CKey, TK, TS, Nonce, Context) ->
    % Check timestamp
    TCheck = z_db:q1("
                SELECT MAX(timestamp) > TIMESTAMP 'epoch' + ($1) * INTERVAL '1 second' AS ok
				FROM oauth_nonce
				WHERE consumer_key = $2
				  AND token        = $3",
                [TS + ?TS_SKEW, CKey, TK],
                Context),
    case TCheck of
        true ->
            {false, <<"Timestamp is out of sequence.">>};
        _FalseOrUndefined ->
            case z_db:q1("
                    SELECT count(*)
                    FROM oauth_nonce
                    WHERE consumer_key = $1
                      AND token = $2
                      AND nonce = $3
                      AND timestamp = TIMESTAMP 'epoch' + ($4) * INTERVAL '1 second'",
                    [CKey, TK, Nonce, TS],
                    Context)
            of
                0 ->
                    z_db:q("
                        INSERT INTO oauth_nonce (consumer_key, token, timestamp, nonce)
                        VALUES ($1, $2, TIMESTAMP 'epoch' + ($3) * INTERVAL '1 second', $4)",
                        [CKey, TK, TS, Nonce],
                        Context),
                    %% Clean up all timestamps older than the one we just received
                    z_db:q("
                        DELETE FROM oauth_nonce
                        WHERE consumer_key = $1
                          AND token = $2
                          AND timestamp < TIMESTAMP 'epoch' + $3 * INTERVAL '1 second'",
                        [CKey, TK, TS-?TS_SKEW],
                        Context),
                    true;
                N when N >= 1 ->
                    {false, <<"Duplicate timestamp/nonce combination, possible replay attack. Request rejected.">>}
            end
    end.


generate_key() ->
    z_ids:id(30).

%%
%% Create a request token for given consumer.
%%
request_token(Consumer, Context) ->
    case z_db:insert(
                oauth_application_token,
                [ {application_id, proplists:get_value(id, Consumer)},
                  {user_id, 1},
                  {token, generate_key()},
                  {token_secret, generate_key()},
                  {token_type, <<"request">>},
                  {callback_uri, proplists:get_value(callback_uri, Consumer)}
                ],
                Context)
    of
        {ok, Id} ->
            z_db:select("oauth_application_token", Id, Context);
        _ ->
            undefined
    end.

authorize_request_token(Token, UserId, Context) ->
    z_db:update("oauth_application_token", proplists:get_value(id, Token), [{authorized, true}, {user_id, UserId}], Context).


exchange_request_for_access(Token, Context) ->
    TokenId = proplists:get_value(id, Token),
    case z_db:q1("
            SELECT id
            FROM oauth_application_token
            WHERE application_id = $1
              AND token = $2
              AND token_type = 'request'
              AND authorized = true",
            [
                proplists:get_value(application_id, Token),
                proplists:get_value(token, Token)
            ],
            Context)
    of
        TokenId ->
            z_db:update(oauth_application_token, TokenId,
                        [{token, generate_key()},
                         {token_secret, generate_key()},
                         {token_type, <<"access">>}], Context),
            z_db:q("UPDATE oauth_application_token SET timestamp = now() WHERE id = $1", [TokenId], Context),
            z_db:select("oauth_application_token", TokenId, Context);

        _UndefinedOrOther ->
            {false, "Failed to exchange request token for access token.\n"}
    end.

%%
%% Create an anonymous request token for given consumer.
%%
ensure_anonymous_token(ConsumerId, Context) when is_integer(ConsumerId) ->
    {ok, AppTkId} = case z_db:q1("
        select id
        from oauth_application_token
        where application_id = $1
          and token like '- - %'",
        [ConsumerId],
        Context)
    of
        undefined ->
            z_db:insert(
                oauth_application_token,
                [ {application_id, ConsumerId},
                  {authorized, true},
                  {user_id, 1},
                  {token, <<"- - ", (generate_key())/binary>>},
                  {token_secret, <<>>},
                  {token_type, <<"access">>},
                  {callback_uri, <<>>}],
                Context);
        Id ->
            {ok, Id}
    end,
    {ok, get_app_tokens(AppTkId, Context)};
ensure_anonymous_token(ConsumerKey, Context) ->
    case z_db:q1("
        select id from oauth_application_registry
        where consumer_key = $1",
        [ConsumerKey],
        Context)
    of
        undefined -> {error, notfound};
        ConsumerId -> ensure_anonymous_token(ConsumerId, Context)
    end.

%%
%% Create an application for a consumer associated with the current user.
%%
create_app(ConsumerId, Context) ->
    create_app(ConsumerId, z_acl:user(Context), Context).

create_app(ConsumerId, UserId, Context) when is_integer(ConsumerId), is_integer(UserId) ->
    Consumer = get_consumer(ConsumerId, Context),
    {ok, AppId} = z_db:insert(
                oauth_application_token,
                [
                    {application_id, ConsumerId},
                    {user_id, UserId},
                    {token, generate_key()},
                    {token_secret, generate_key()},
                    {token_type, <<"access">>},
                    {authorized, true},
                    {callback_uri, proplists:get_value(callback_uri, Consumer)}
                ],
                Context),
    {ok, get_app_tokens(AppId, Context)}.

get_app_tokens(AppId, Context) ->
    Ts = z_db:assoc_row("
                select cons.consumer_key, cons.consumer_secret, tk.token, tk.token_secret
                from oauth_application_token tk join
                     oauth_application_registry cons
                     on tk.application_id = cons.id
                where tk.id = $1",
                [AppId],
                Context),
    case proplists:get_value(token, Ts) of
        <<"- - ", _/binary>> ->
            proplists:delete(token,
                proplists:delete(token_secret, Ts));
        _ -> Ts
    end.

%%
%% Get a token from the database.oauth_application_registry
%%
get_request_token(Tok, Context) ->
    z_db:assoc_row("
        SELECT *
        FROM oauth_application_token tok
            JOIN oauth_application_registry app
              ON tok.application_id = app.id
        WHERE tok.token = $1
        LIMIT 1",
        [Tok],
        Context).


create_consumer(Title, Context) ->
    create_consumer(Title, <<>>, <<>>, <<>>, Context).

create_consumer(Title, URL, Desc, Callback, Context) ->
    case z_db:insert(
                oauth_application_registry,
                [ {user_id, z_acl:user(Context)},
                  {consumer_key, generate_key()},
                  {consumer_secret, generate_key()},
                  {enabled, true},
                  {callback_uri, Callback},
                  {application_title, Title},
                  {application_uri, URL},
                  {application_descr, Desc},
                  {application_notes, ""},
                  {application_type, ""}], Context) of
        {ok, Id} ->
            {ok, C} = z_db:select(oauth_application_registry, Id, Context),
            C;
        _ ->
            undefined
    end.

update_consumer(Id, Update, Context) ->
    z_db:update("oauth_application_registry", Id, Update, Context).

get_consumer(Id, Context) ->
    {ok, C} = z_db:select(oauth_application_registry, Id, Context),
    C.

delete_consumer(Id, Context) ->
    {ok, _} = z_db:delete(oauth_application_registry, Id, Context).

delete_consumers(Context) ->
    _N = z_db:q("delete from oauth_application_registry", Context),
    ok.

reset_consumer_secret(Id, Context) ->
    Secret = generate_key(),
    _ = z_db:update(oauth_application_registry, Id, [{consumer_secret, Secret}], Context),
    {ok, Secret}.
