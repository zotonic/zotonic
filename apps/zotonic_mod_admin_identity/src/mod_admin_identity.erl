%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2013 Marc Worrell
%% @doc Identity administration.  Adds overview of users to the admin and enables to add passwords on the edit page.

%% Copyright 2009-2013 Marc Worrell
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

-module(mod_admin_identity).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Admin identity/user supports").
-mod_description("Adds support for handling and verification of user identities.").
-mod_depends([admin]).
-mod_provides([]).


%% interface functions
-export([
    observe_identity_verified/2,
    observe_identity_password_match/2,
    observe_rsc_update/3,
    observe_search_query/2,
    observe_admin_menu/3,
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").


observe_identity_verified(#identity_verified{user_id=RscId, type=Type, key=Key}, Context) ->
    m_identity:set_verified(RscId, Type, Key, Context).


observe_identity_password_match(#identity_password_match{password=Password, hash=Hash}, _Context) ->
    case m_identity:hash_is_equal(Password, Hash) of
        true ->
            case m_identity:needs_rehash(Hash) of
                true ->
                    {ok, rehash};
                false ->
                    ok
            end;
        false ->
            {error, password}
    end.


observe_rsc_update(#rsc_update{action=Action, id=RscId, props=Pre}, {_Modified, Post} = Acc, Context)
    when Action =:= insert; Action =:= update ->
    case z_context:get(is_m_identity_update, Context) of
        true ->
            Acc;
        _false ->
            case {proplists:get_value(email, Pre), proplists:get_value(email, Post)} of
                {A, A} -> Acc;
                {_Old, undefined} -> Acc;
                {_Old, <<>>} -> Acc;
                {_Old, New} ->
                    ensure(RscId, email, z_html:unescape(New), Context),
                    Acc
            end
    end;
observe_rsc_update(#rsc_update{}, Acc, _Context) ->
    Acc.


observe_search_query({search_query, Req, OffsetLimit}, Context) ->
    search(Req, OffsetLimit, Context).

observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id=admin_user,
                parent=admin_auth,
                label=?__("Users", Context),
                url={admin_user},
                visiblecheck={acl, use, mod_admin_identity}}

     |Acc].


% Verify an identity - for now assume an e-mail identity
event(#postback{message={identity_verify_confirm, Args}}, Context) ->
    {idn_id, IdnId} = proplists:lookup(idn_id, Args),
    case m_identity:get(IdnId, Context) of
        undefined ->
            z_render:growl_error("Sorry, can not find this identity.", Context);
        Idn ->
            z_render:wire({confirm, [
                            {text, [
                                    ?__("This will send a verification e-mail to ", Context),
                                    proplists:get_value(key, Idn), $.
                                ]},
                            {ok, ?__("Send", Context)},
                            {action, {postback, [
                                        {postback, {identity_verify, Args}},
                                        {delegate, ?MODULE}
                                    ]}}
                        ]},
                        Context)
    end;
event(#postback{message={identity_verify, Args}}, Context) ->
    {id, RscId} = proplists:lookup(id, Args),
    {idn_id, IdnId} = proplists:lookup(idn_id, Args),
    case send_verification(RscId, IdnId, Context) of
        {error, notfound} ->
            z_render:growl_error("Sorry, can not find this identity.", Context);
        {error, unsupported} ->
            z_render:growl_error("Sorry, can not verify this identity.", Context);
        ok ->
            z_render:growl(?__("Sent verification e-mail.", Context), Context)
    end;
event(#postback{message={identity_verify_check, Args}}, Context) ->
    {idn_id, IdnId} = proplists:lookup(idn_id, Args),
    {verify_key, VerifyKey} = proplists:lookup(verify_key, Args),
    Context1 = z_render:wire({hide, [{target, "verify-checking"}]}, Context),
    case verify(IdnId, VerifyKey, Context1) of
        {error, notfound} ->
            z_render:wire({fade_in, [{target, "verify-error"}]}, Context1);
        ok ->
            z_render:wire({fade_in, [{target, "verify-ok"}]}, Context1)
    end;
event(#postback{message={identity_verify_preferred, Args}}, Context) ->
    {id, RscId} = proplists:lookup(id, Args),
    {type, Type} = proplists:lookup(type, Args),
    Key = z_context:get_q(<<"key">>, Context),
    case m_rsc:is_editable(RscId, Context) of
        true ->
            case Type of
                "email" ->
                    case Key /= undefined andalso z_email_utils:is_email(Key) of
                        true ->
                            % Set the email property of the resource
                            {ok, _} = m_rsc:update(RscId, [{email, Key}], Context),
                            Context;
                        false ->
                            z_render:growl_error(?__("This is not a valid e-mail address.", Context), Context)
                    end;
                _ ->
                    % Ignore - don't know what to do here
                    Context
            end;
        false ->
            z_render:growl_error(?__("You are not allowed to edit identities.", Context), Context)
    end;

% Delete an identity
event(#postback{message={identity_delete_confirm, Args}}, Context) ->
    z_render:wire({confirm, [
                    {text, ?__("Are you sure you want to delete this entry?", Context)},
                    {ok, ?__("Delete", Context)},
                    {action, {postback, [
                                {postback, {identity_delete, Args}},
                                {delegate, ?MODULE}
                            ]}}
                ]},
                Context);
event(#postback{message={identity_delete, Args}}, Context) ->
    {id, RscId} = proplists:lookup(id, Args),
    {idn_id, IdnId} = proplists:lookup(idn_id, Args),
    {list_element, ListId} = proplists:lookup(list_element, Args),
    case m_rsc:is_editable(RscId, Context) of
        true ->
            case m_identity:get(IdnId, Context) of
                undefined -> nop;
                Idn -> {rsc_id, RscId} = proplists:lookup(rsc_id, Idn)
            end,
            {ok, _} = m_identity:delete(IdnId, Context),
            z_render:wire({mask, [{target, ListId}]}, Context);
        false ->
            z_render:growl_error(?__("You are not allowed to edit identities.", Context), Context)
    end;

% Add an identity
event(#postback{message={identity_add, Args}}, Context) ->
    {id, RscId} = proplists:lookup(id, Args),
    case m_rsc:is_editable(RscId, Context) of
        true ->
            Type = z_convert:to_atom(proplists:get_value(type, Args, email)),
            case z_string:trim(z_context:get_q(<<"idn-key">>, Context, <<>>)) of
                <<>> ->
                    Context;
                Key ->
                    KeyNorm = m_identity:normalize_key(Type, Key),
                    case m_identity:is_valid_key(Type, KeyNorm, Context) of
                        true ->
                            case is_existing_key(RscId, Type, KeyNorm, Context) of
                                true ->
                                    ignore;
                                false ->
                                    {ok, _IdnId} = m_identity:insert(RscId, Type, KeyNorm, Context)
                            end,
                            Context;
                        false ->
                            case proplists:get_value(error_target, Args) of
                                undefined ->
                                    z_render:growl(?__("The address is invalid.", Context), Context);
                                ErrorTarget ->
                                    z_render:wire({add_class, [{target, ErrorTarget}, {class, "has-error"}]}, Context)
                            end
                    end
            end;
        false ->
            z_render:growl_error(?__("You are not allowed to edit identities.", Context), Context)
    end;

%% Log on as this user
event(#postback{message={switch_user, [{id, Id}]}}, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            % Changing the authenticated will force all connected pages to reload or change.
            % After this we can't send any replies any more, as the pages are disconnecting.
            {ok, NewContext} = z_auth:switch_user(Id, Context),
            NewContext;
        false ->
            z_render:growl_error(?__("You are not allowed to switch users.", Context), Context)
    end.




is_existing_key(RscId, Type, Key, Context) ->
    Existing = m_identity:get_rsc_by_type(RscId, Type, Context),
    case lists:filter(fun(Idn) -> proplists:get_value(key, Idn) == Key end, Existing) of
        [] -> false;
        _ -> true
    end.

ensure(_RscId, _Type, undefined, _Context) -> ok;
ensure(_RscId, _Type, <<>>, _Context) -> ok;
ensure(_RscId, _Type, [], _Context) -> ok;
ensure(RscId, Type, Key, Context) ->
    m_identity:insert(RscId, Type, Key, Context).


%%====================================================================
%% support functions
%%====================================================================


send_verification(RscId, IdnId, Context) ->
    case m_identity:get(IdnId, Context) of
        undefined ->
            {error, notfound};
        Idn ->
            {rsc_id, RscId} = proplists:lookup(rsc_id, Idn),
            case proplists:get_value(type, Idn) of
                <<"email">> ->
                    % Send the verfication e-mail
                    Email = proplists:get_value(key, Idn),
                    {ok, VerifyKey} = m_identity:set_verify_key(IdnId, Context),
                    Vars = [
                        {idn, Idn},
                        {id, RscId},
                        {verify_key, VerifyKey}
                    ],
                    z_email:send_render(Email, "email_identity_verify.tpl", Vars, Context),
                    ok;
                _Type ->
                    {error, unsupported}
            end
    end.


verify(IdnId, VerifyKey, Context) ->
    case m_identity:lookup_by_verify_key(VerifyKey, Context) of
        undefined ->
            % Return ok when the identity was already verified
            case catch z_convert:to_integer(IdnId) of
                N when is_integer(N) ->
                    case m_identity:get(N, Context) of
                        undefined ->
                            {error, notfound};
                        Idn ->
                            case z_convert:to_bool(proplists:get_value(is_verified, Idn)) of
                                true -> ok;
                                false -> {error, wrongkey}
                            end
                    end;
                _ ->
                    {error, notfound}
            end;
        Idn ->
            % Set the identity to verified
            IdnIdBin = z_convert:to_binary(IdnId),
            case z_convert:to_binary(proplists:get_value(id, Idn)) of
                IdnIdBin ->
                    m_identity:set_verified(proplists:get_value(id, Idn), Context),
                    ok;
                _ ->
                    {error, wrongkey}
            end
    end.


search({users, []}, OffsetLimit, Context) ->
    search({user, [{text,undefined},{users_only,true}]}, OffsetLimit, Context);
search({users, [{users_only,UsersOnly}]}, OffsetLimit, Context) ->
    search({user, [{text,undefined},{users_only,UsersOnly}]}, OffsetLimit, Context);
search({users, [{text,Text}]}, OffsetLimit, Context) ->
    search({user, [{text,Text},{user_only,true}]}, OffsetLimit, Context);
search({users, [{text,QueryText}, {users_only, UsersOnly0}]}, _OffsetLimit, Context) ->
    UsersOnly = z_convert:to_bool(UsersOnly0),
    {TSJoin, Where, Args, Order} = case z_utils:is_empty(QueryText) of
                        true ->
                            {[], [], [], "r.pivot_title"};
                        false ->
                            {", plainto_tsquery($2, $1) query",
                             "query @@ r.pivot_tsv",
                             [QueryText, z_pivot_rsc:stemmer_language(Context)],
                             "ts_rank_cd(pivot_tsv, query, 32)"}
                     end,
    IdnJoin = case UsersOnly of
                true -> " join identity i on (r.id = i.rsc_id and i.type = 'username_pw') ";
                false -> ""
              end,
    Cats = case UsersOnly of
                true -> [];
                false -> [{"r", [person, institution]}]
           end,
    #search_sql{
       select="r.id",
       from="rsc r " ++ IdnJoin ++ TSJoin,
       where=Where,
       order=Order,
       args=Args,
       cats=Cats,
       tables=[{rsc,"r"}]
      };
search(_, _, _) ->
    undefined.


