%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2022 Marc Worrell
%% @doc Let new members register themselves.

%% Copyright 2010-2022 Marc Worrell
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

-module(mod_signup).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Sign up users").
-mod_description("Implements public sign up to register as member of this site.").
-mod_prio(500).
-mod_schema(1).
-mod_depends([ base, mod_authentication, mod_server_storage ]).
-mod_provides([signup]).


-export([
    manage_schema/2,

    observe_signup/2,
    observe_signup_url/2,
    observe_identity_verification/2,
    observe_logon_ready_page/2,

    signup/4,
    signup_existing/5,
    request_verification/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").


%% @doc Add a new user or an existing person as user.
observe_signup(#signup{id=UserId, props=Props, signup_props=SignupProps, request_confirm=RequestConfirm}, Context) ->
    signup_existing(UserId, Props, SignupProps, RequestConfirm, Context).


%% @doc Check if a module wants to redirect to the signup form.  Returns either {ok, Location} or undefined.
observe_signup_url(#signup_url{props=Props, signup_props=SignupProps}, Context) ->
    CheckId = z_ids:id(),
    ok = m_server_storage:secure_store(CheckId, {CheckId, Props, SignupProps}, Context),
    {ok, z_dispatcher:url_for(signup, [{xs, CheckId}], Context)}.


observe_identity_verification(#identity_verification{user_id=UserId, identity=undefined}, Context) ->
    request_verification(UserId, Context);
observe_identity_verification(#identity_verification{user_id=UserId, identity=Ident}, Context) ->
    case proplists:get_value(type, Ident) of
        <<"email">> -> send_verify_email(UserId, Ident, Context);
        _ -> false
    end.


%% @doc Return the url to redirect to when the user logged on, defaults to the user's personal page.
observe_logon_ready_page(#logon_ready_page{ request_page = None }, Context) when None =:= undefined; None =:= <<>> ->
    case z_auth:is_auth(Context) of
        true -> m_rsc:p(z_acl:user(Context), page_url, Context);
        false -> undefined
    end;
observe_logon_ready_page(#logon_ready_page{ request_page = _ }, _Context) ->
    undefined.


%% @doc Sign up a new user.
-spec signup(list(), list() | map(), boolean(), z:context()) -> {ok, integer()} | {error, term()}.
signup(Props, SignupProps, RequestConfirm, Context) ->
    signup_existing(undefined, Props, SignupProps, RequestConfirm, Context).

%% @doc Sign up a existing user
-spec signup_existing(integer()|undefined, map() | list(), list(), boolean(), z:context()) -> {ok, integer()} | {error, term()}.

signup_existing(UserId, Props, SignupProps, RequestConfirm, Context) when is_list(Props) ->
    {ok, PropsMap} = z_props:from_list(Props),
    signup_existing(UserId, PropsMap, SignupProps, RequestConfirm, Context);
signup_existing(UserId, Props, SignupProps, RequestConfirm, Context) when is_map(Props) ->
    case check_signup(Props, SignupProps, Context) of
        {ok, Props1, SignupProps1} ->
            do_signup(UserId, Props1, SignupProps1, RequestConfirm, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Sent verification requests to non verified identities
request_verification(UserId, Context) ->
    Unverified = [ R || R <- m_identity:get_rsc(UserId, Context), proplists:get_value(is_verified, R) == false ],
    request_verification(UserId, Unverified, false, Context).

request_verification(_, [], false, _Context) ->
    {error, no_verifiable_identities};
request_verification(_, [], true, _Context) ->
    ok;
request_verification(UserId, [Ident|Rest], Requested, Context) ->
    case z_notifier:first(#identity_verification{user_id=UserId, identity=Ident}, Context) of
        ok -> request_verification(UserId, Rest, true, Context);
        _ -> request_verification(UserId, Rest, Requested, Context)
    end.

%%====================================================================
%% support functions
%%====================================================================

%% @doc Preflight checks on a signup
%% This function is called with a 'sudo' context.
check_signup(Props, SignupProps, Context) ->
    case z_notifier:foldl(signup_check, {ok, Props, SignupProps}, Context) of
        {ok, Props1, SignupProps1} ->
            UserId = proplists:get_value(user_id, SignupProps),
            case check_identity(UserId, SignupProps1, Context) of
                ok ->
                    {ok, Props1, SignupProps1};
                {error, _} = Error ->
                    Error
            end;
        {error, _ContextOrReason} = Error ->
            Error
    end.

%% @doc Preflight check on identities, prevent double identity keys.
check_identity(_UserId, [], _Context) ->
    ok;
check_identity(UserId, [{identity, {username_pw, {Username, _Password}, true, _Verified}}|Idents], Context) ->
    case username_exists(UserId, Username, Context) of
        false -> check_identity(UserId, Idents, Context);
        true -> {error, {identity_in_use, username}}
    end;
check_identity(UserId, [{identity, {Type, Key, true, _Verified}}|Idents], Context) ->
    case identity_exists(UserId, Type, Key, Context) of
        false -> check_identity(UserId, Idents, Context);
        true -> {error, {identity_in_use, Type}}
    end;
check_identity(UserId, [_|Idents], Context) ->
    check_identity(UserId, Idents, Context).


%% @doc Insert or update a new user, return the user id on success. Assume all args are ok as we did
%% a preflight check and users sign up slowly (ie. no race condition)
do_signup(UserId, Props, SignupProps, RequestConfirm, Context) ->
    IsVerified = not RequestConfirm orelse has_verified_identity(SignupProps),
    case insert_or_update(UserId, props_to_rsc(Props, IsVerified, Context), Context) of
        {ok, NewUserId} ->
            ContextLogon = z_acl:logon(NewUserId, Context),
            ContextUser = case m_rsc:p_no_acl(NewUserId, creator_id, Context) of
                NewUserId -> z_acl:sudo(ContextLogon);
                _ -> ContextLogon
            end,
            ensure_identities(NewUserId, SignupProps, ContextUser),
            z_notifier:map(#signup_done{id=NewUserId, is_verified=IsVerified, props=Props, signup_props=SignupProps}, ContextUser),
            case IsVerified of
                true -> z_notifier:map(#signup_confirm{id=NewUserId}, ContextUser);
                false -> nop
            end,
            maybe_add_depiction(NewUserId, Props, ContextUser),
            {ok, NewUserId};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Optionally add a depiction using the 'depiction_url' in the user's props
-spec maybe_add_depiction( UserId :: m_rsc:resource_id(), map(), z:context() ) -> ok | {error, term()}.
maybe_add_depiction(Id, #{ <<"depiction_url">> := Url }, ContextUser)
    when Url =/= <<>>, Url =/= "", Url =/= undefined ->
    case z_convert:to_bool( m_config:get_value(mod_signup, depiction_as_medium, ContextUser) ) of
        false ->
            case m_edge:objects(Id, depiction, ContextUser) of
                [] ->
                    MediaProps = #{
                        <<"is_dependent">> => true,
                        <<"is_published">> => true,
                        <<"content_group_id">> => m_rsc:p_no_acl(Id, content_group_id, ContextUser)
                    },
                    case m_media:insert_url(Url, MediaProps, ContextUser) of
                        {ok, MediaId} ->
                            ?LOG_INFO(#{
                                text => <<"Added user depiction from depiction_url">>,
                                in => zotonic_mod_signup,
                                result => ok,
                                user_id => Id,
                                rsc_id => MediaId,
                                url => Url
                            }),
                            {ok, _} = m_edge:insert(Id, depiction, MediaId, ContextUser),
                            ok;
                        {error, Reason} = Error ->
                            ?LOG_WARNING(#{
                                text => <<"Could not insert user depiction_url">>,
                                in => zotonic_mod_signup,
                                user_id => Id,
                                result => error,
                                reason => Reason,
                                url => Url
                            }),
                            Error
                    end;
                _ ->
                    ok
            end;
        true ->
            case m_media:get(Id, ContextUser) of
                undefined ->
                    case m_media:replace_url(Url, Id, #{}, ContextUser) of
                        {ok, _Id} ->
                            ?LOG_INFO(#{
                                text => <<"Added medium from depiction_url for user">>,
                                in => zotonic_mod_signup,
                                result => ok,
                                user_id => Id,
                                rsc_id => Id,
                                url => Url
                            }),
                            ok;
                        {error, Reason} = Error ->
                            ?LOG_WARNING(#{
                                text => <<"Could not set user medium from depiction_url">>,
                                in => zotonic_mod_signup,
                                user_id => Id,
                                result => error,
                                reason => Reason,
                                url => Url
                            }),
                            Error
                    end;
                _Medium ->
                    ok
            end
    end;
maybe_add_depiction(_Id, _Props, _ContextUser) ->
    ok.

% First insert a minimal user, before performing the complete insert
% using the rights of the user.
insert_or_update(undefined, Props, Context) ->
    Ks = [
        <<"is_published">>,
        <<"is_verified_account">>,
        <<"creator_id">>,
        <<"category_id">>,
        <<"content_group_id">>,
        <<"visible_for">>,
        <<"pref_language">>,
        <<"pref_tz">>,
        <<"title">>,
        <<"name_first">>,
        <<"name_surname_prefix">>,
        <<"name_surname">>,
        <<"email">>
    ],
    InsertProps = maps:with(Ks, Props),
    case m_rsc:insert(InsertProps, z_acl:sudo(Context)) of
        {ok, UserId} ->
            UpdateProps = maps:without(Ks, Props),
            case maps:size(UpdateProps) of
                0 ->
                    {ok, UserId};
                _ ->
                    m_rsc:update(UserId, UpdateProps, z_acl:logon(UserId, Context))
            end;
        {error, _} = Error ->
            Error
    end;
insert_or_update(UserId, Props, Context) when is_integer(UserId) ->
    m_rsc:update(UserId, Props, z_acl:logon(UserId, Context)).


has_verified_identity([]) -> false;
has_verified_identity([{identity, {Type, _, _, true}}|_Is]) when Type /= username_pw -> true;
has_verified_identity([_|Is]) -> has_verified_identity(Is).


ensure_identities(Id, SignupProps, Context) ->
    [ ensure_identity(Id, Ident, Context) || {K,Ident} <- SignupProps, K == identity ].

ensure_identity(Id, {username_pw, {Username, Password}, true, true}, Context) ->
    case m_identity:set_username_pw(Id, Username, Password, Context) of
        ok -> ok;
        Error -> throw(Error)
    end;
ensure_identity(Id, {Type, Key, IsUnique, IsVerified}, Context) when is_binary(Key); is_list(Key) ->
    m_identity:insert(Id, Type, Key, [{is_verified, IsVerified}, {is_unique, IsUnique}], Context).


props_to_rsc(Props, IsVerified, Context) when is_list(Props) ->
    {ok, PropsMap} = z_props:from_list(Props),
    props_to_rsc(PropsMap, IsVerified, Context);
props_to_rsc(Props, IsVerified, Context) when is_map(Props) ->
    Category = z_convert:to_atom(m_config:get_value(mod_signup, member_category, person, Context)),
    ContentGroup = z_convert:to_atom(m_config:get_value(mod_signup, content_group, undefined, Context)),
    Props1 = maps:without([
            <<"category">>,
            <<"content_group">>
        ], Props),
    Props2 = Props1#{
        <<"is_published">> => IsVerified,
        <<"content_group_id">> => ContentGroup,
        <<"category_id">> => Category,
        <<"is_verified_account">> => IsVerified,
        <<"creator_id">> => self,
        <<"pref_language">> => z_context:language(Context)
    },
    case maps:is_key(<<"title">>, Props2) of
        true ->
            Props2;
        false ->
            Vs = [
                {id, Props2}
            ],
            {Title, _} = z_template:render_to_iolist("_name.tpl", Vs, Context),
            Props2#{
                <<"title">> => iolist_to_binary(Title)
            }
    end.


%% @doc Check if a username exists
username_exists(UserId, Username, Context) ->
    case m_identity:is_reserved_name(Username) of
        true ->
            true;
        false ->
            case m_identity:lookup_by_username(Username, Context) of
                undefined -> false;
                Props -> UserId =/= proplists:get_value(rsc_id, Props)
            end
    end.


%% @doc Check if the identity exists
identity_exists(UserId, Type, Key, Context) ->
    case m_identity:lookup_by_type_and_key(Type, Key, Context) of
        undefined -> false;
        Props -> UserId =/= proplists:get_value(rsc_id, Props)
    end.


send_verify_email(UserId, Ident, Context) ->
    Email = proplists:get_value(key, Ident),
    {ok, Key} = m_identity:set_verify_key(proplists:get_value(id, Ident), Context),
    Vars = [
        {recipient_id, UserId},
        {user_id, UserId},
        {email, Email},
        {verify_key, Key}
    ],
    z_email:send_render(UserId, "email_verify.tpl", Vars, z_acl:sudo(Context)),
    ok.


manage_schema(install, _Context) ->
    #datamodel{
        resources=[
            {signup_tos, text, #{
                <<"is_published">> => true,
                <<"page_path">> => <<"/terms">>,
                <<"title">> => <<"Terms of Service">>,
                <<"summary">> => <<"These Terms of Service (\"Terms\") govern your access to and use of the services and COMPANY’s web sites (the \"Services\"), and any information, text, graphics, or other materials uploaded, downloaded or appearing on the Services (collectively referred to as \"Content\"). Your access to and use of the Services is conditioned on your acceptance of and compliance with these Terms. By accessing or using the Services you agree to be bound by these Terms."/utf8>>,
                <<"body">> => <<"<h2>INSERT YOUR TERMS OF SERVICE HERE</h2>">>
            }},
            {signup_privacy, text, #{
                <<"is_published">> => true,
                <<"page_path">> => <<"/privacy">>,
                <<"title">> => <<"Privacy Policy">>,
                <<"summary">> => <<"This Privacy Policy describes COMPANY’s policies and procedures on the collection, use and disclosure of your information. COMPANY receives your information through our various web sites, SMS, APIs, services and third-parties (\"Services\"). When using any of our Services you consent to the collection, transfer, manipulation, storage, disclosure and other uses of your information as described in this Privacy Policy. Irrespective of which country that you reside in or create information from, your information may be used by COMPANY in any country where COMPANY operates."/utf8>>,
                <<"body">> => <<"<h2>INSERT YOUR PRIVACY POLICY HERE</h2>">>
            }}
        ]
    }.
