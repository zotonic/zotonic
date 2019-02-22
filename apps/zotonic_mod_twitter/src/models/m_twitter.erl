%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2018 Marc Worrell
%% @doc Model for mod_twitter

%% Copyright 2017-2018 Marc Worrell
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

-module(m_twitter).

-behaviour (zotonic_model).

-export([
    m_get/3,

    is_useauth/1,

    install/2,

    get/2,
    list/1,

    due/2,
    next_due/1,
    set_due/5,
    set_import_count/3,
    disable/3,

    update_config_subscriptions/1,
    update_identitiy_subscription/2,
    update_identitiy_subscriptions/1,

    normalize_key/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").


-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ useauth | Rest ], _Msg, Context) ->
    {ok, {is_useauth(Context), Rest}};
m_get(Vs, _Msg, _Context) ->
    lager:info("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {error, unknown_path}.

-spec is_useauth(z:context()) -> boolean().
is_useauth(Context) ->
    case m_config:get_value(mod_twitter, consumer_key, Context) of
        undefined -> false;
        <<>> -> false;
        _ -> m_config:get_boolean(mod_twitter, useauth, Context)
    end.


%% @doc Fetch a subscriptions
-spec get(integer(), z:context()) -> {ok, list()} | {error, not_found}.
get(SubId, Context) ->
    case z_db:assoc_row("
        select ts.*, idn.rsc_id as user_id
        from twitter_subscriptions ts
            left join identity idn
            on ts.identity_id = idn.id
        where ts.id = $1
        ",
        [ SubId ],
        Context)
    of
        undefined ->
            {error, not_found};
        Row ->
            {ok, Row}
    end.

%% @doc Fetch a list of all twitter subscriptions
-spec list( z:context() ) -> list().
list(Context) ->
    z_db:assoc("
        select ts.*, idn.rsc_id as user_id
        from twitter_subscriptions ts
            left join identity idn
            on ts.identity_id = idn.id
        order by poll_due asc
        ",
        Context).

%% @doc Fetch all polls due, order by ascending due date
-spec due(calendar:datetime(), z:context()) -> list().
due(DT, Context) ->
    z_db:assoc("
        select ts.*, idn.rsc_id as user_id
        from twitter_subscriptions ts
            left join identity idn
            on ts.identity_id = idn.id
        where poll_due <= $1
          and is_enabled
        order by poll_due asc
        ",
        [ DT ],
        Context).

%% @doc Return the next due (if any)
-spec next_due( z:context() ) -> calendar:datetime() | undefined.
next_due(Context) ->
    z_db:q1("
        select min(poll_due)
        from twitter_subscriptions
        where is_enabled",
        Context).

%% @doc Set the new due time for the feed, optionally set import stats.
-spec set_due( integer(), calendar:datetime(), integer() | undefined, integer(), z:context() ) -> ok | {error, not_found}.
set_due(SubId, NewDue, _NewMaxId, 0, Context) ->
    case z_db:q("
        update twitter_subscriptions
        set poll_due = $2
        where id = $1",
        [ SubId, NewDue ],
        Context)
    of
        1 -> ok;
        0 -> {error, not_found}
    end;
set_due(SubId, NewDue, NewMaxId, ImportCount, Context) ->
    case z_db:q("
        update twitter_subscriptions
        set poll_due = $2,
            last_id = $3,
            last_import = now(),
            import_count = import_count + $4
        where id = $1",
        [ SubId, NewDue, NewMaxId, ImportCount ],
        Context)
    of
        1 -> ok;
        0 -> {error, not_found}
    end.

%% @doc Set the import count for the subscription
-spec set_import_count( integer(), integer(), z:context() ) -> ok | {error, not_found}.
set_import_count(SubId, ImportCount, Context) ->
    case z_db:q("
        update twitter_subscriptions
        set last_import = now(),
            import_count = import_count + $2
        where id = $1",
        [ SubId, ImportCount ],
        Context)
    of
        1 -> ok;
        0 -> {error, not_found}
    end.

%% @doc Disable a feed
disable(SubscriptionId, Reason, Context) when is_integer(SubscriptionId) ->
    case z_db:q("
        update twitter_subscriptions
        set is_enabled = false
            last_error = $2
        where id = $1",
        [ SubscriptionId, Reason ],
        Context)
    of
        1 -> ok;
        0 -> {error, not_found}
    end.

%% @doc Update a single identity subscription
update_identitiy_subscription(RscId, Context) ->
    case z_db:q_row("
        select id, lower(key)
        from identity
        where type = 'twitter_id'
          and rsc_id = $1",
        [ RscId ],
        Context)
    of
        undefined ->
            ok;
        {IdnId, Key} ->
            Key1 = normalize_key(Key, user),
            update_identitiy_subscription_1(IdnId, Key1, Context)
    end.

update_identitiy_subscription_1(IdnId, K, Context) ->
    case z_db:q1("
        select is_enabled, key
        from twitter_subscriptions
        where identity_id = $1",
        [ IdnId ],
        Context)
    of
        undefined ->
            z_db:insert(
                twitter_subscriptions,
                [
                    {key, K},
                    {identity_id, IdnId}
                ],
                Context);
        {true, K} ->
            ok;
        _ ->
            z_db:q("
                update twitter_subscriptions
                set is_enabled = true,
                    key = $1,
                    poll_due = now()
                where identity_id = $2",
                [ K, IdnId ],
                Context)
    end.


%% @doc Copy all twitter identities over to the subscriptions table, fk to identity record
update_identitiy_subscriptions(Context) ->
    Ks = z_db:q("
        select distinct id, lower(key)
        from identity
        where type = 'twitter_id'",
        Context),
    Ks1 = lists:map(
        fun ({IdnId, K}) ->
            {IdnId, normalize_key(K, user)}
        end,
        Ks),
    All = z_db:q("
        select identity_id, key
        from twitter_subscriptions
        where identity_id is not null",
        Context),
    lists:foreach(
        fun
            ({_IdnId, <<>>}) ->
                ok;
            ({IdnId, K}) ->
                update_identitiy_subscription_1(IdnId, K, Context)
        end,
        Ks1),
    lists:foreach(
        fun({IdnId, K}) ->
            z_db:q("
                delete from twitter_subscriptions
                where key = $1
                  and identity_id = $2",
                [ IdnId, K ],
                Context)
        end,
        All -- Ks1).

update_config_subscriptions(Context) ->
    Config = m_config:get_value(mod_twitter, follow, Context),
    update_config_subscriptions(Config, Context).

%% @doc Sync the subscriptions table, remove all subs
update_config_subscriptions(S, Context) when is_list(S) ->
    update_config_subscriptions(z_convert:to_binary(S), Context);
update_config_subscriptions(S, Context) ->
    Keys = lists:map( fun(K) -> normalize_key(K, any) end, split(S) ),
    All = z_db:q("
        select key
        from twitter_subscriptions
        where identity_id is null",
        Context),
    All1 = [ K || {K} <- All ],
    lists:foreach(
        fun
            (<<>>) ->
                ok;
            (K) ->
                case z_db:q1("
                    select is_enabled
                    from twitter_subscriptions
                    where key = $1
                      and identity_id is null",
                    [ K ],
                    Context)
                of
                    undefined ->
                        z_db:insert(
                            twitter_subscriptions,
                            [
                                {key, K}
                            ],
                            Context);
                    true ->
                        ok;
                    false ->
                        z_db:q("
                            update twitter_subscriptions
                            set is_enabled = true
                            where key = $1
                              and identity_id is null
                              and not is_enabled",
                            [ K ],
                            Context)
                end
        end,
        Keys),
    lists:foreach(
        fun(K) ->
            z_db:q("
                delete from twitter_subscriptions
                where key = $1
                  and identity_id is null",
                [ K ],
                Context)
        end,
        All1 -- Keys).

-spec normalize_key( string() | binary() | undefined, user | any ) -> binary().
normalize_key(undefined, _Type) ->
    <<>>;
normalize_key(<<>>, _Type) ->
    <<>>;
normalize_key(K, Type) when not is_binary(K) ->
    normalize_key(z_convert:to_binary(K), Type);
normalize_key(K, user) when is_binary(K) ->
    K1 = z_string:trim(K),
    case z_utils:only_digits(K1) of
        true ->
            z_string:truncate(<<"@#", K1/binary>>, 126, <<>>);
        false ->
            K2 = case K1 of
                <<"@", _/binary>> -> K1;
                _ -> <<"@", K1/binary>>
            end,
            normalize_key(K2, any)
    end;
normalize_key(K, any) ->
    K1 = normalize_key_1( z_string:trim(K) ),
    K2 = z_string:to_lower(K1),
    z_string:truncate(K2, 126, <<>>).

normalize_key_1(<<"@", _/binary>> = Username) -> Username;
normalize_key_1(<<"#", _/binary>> = Tag) -> Tag;
normalize_key_1(Phrase) -> Phrase.

split(undefined) ->
    [];
split(B) when is_binary(B) ->
    B1 = binary:replace(B, <<9>>, <<" ">>, [global]),
    B2 = re:split(B1, <<"[ \n\r\f,]">>),
    [ z_string:trim(F) || F <- B2 ].


%% @doc Install the support tables for the twitter poller.
install(_Version, Context) ->
    case z_db:table_exists(twitter_subscriptions, Context) of
        false ->
            [] = z_db:q("
                create table twitter_subscriptions (
                    id serial not null,
                    is_enabled bool not null default true,
                    identity_id int,
                    key character varying(128) not null,
                    created timestamp with time zone NOT NULL DEFAULT now(),
                    modified timestamp with time zone NOT NULL DEFAULT now(),
                    poll_due timestamp with time zone NOT NULL DEFAULT now(),
                    last_error character varying(300),
                    last_id bigint not null default 0,
                    last_import timestamp with time zone,
                    import_count int not null default 0,

                    CONSTRAINT twitter_subscriptions_pkey PRIMARY KEY (id)
                )",
                Context),
            [] = z_db:q(
                "ALTER TABLE twitter_subscriptions
                 ADD CONSTRAINT fk_twitter_subscriptions_identity_id FOREIGN KEY (identity_id)
                 REFERENCES identity (id)
                 ON UPDATE CASCADE ON DELETE CASCADE",
                Context),
            [] = z_db:q(
                "CREATE INDEX fki_twitter_subscriptions_identity_id ON twitter_subscriptions (identity_id)",
                Context),
            [] = z_db:q(
                "CREATE INDEX twitter_subscriptions_poll_due_key ON twitter_subscriptions (poll_due)",
                Context),
            [] = z_db:q(
                "CREATE INDEX twitter_subscriptions_key ON twitter_subscriptions (key)",
                Context),
            z_db:flush(Context),
            update_config_subscriptions(Context),
            update_identitiy_subscriptions(Context);
        true ->
            ok
    end.
