%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-05-12
%% @doc Let new members register themselves.
%% @todo Check person props before sign up
%% @todo Add verification and verification e-mails (check for _Verified, add to m_identity)

%% Copyright 2010 Marc Worrell
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
-mod_depends([base, authentication]).
-mod_provides([signup]).


-export([
    manage_schema/2,

    observe_signup_url/2,
    observe_identity_verification/2,
    observe_logon_ready_page/2
]).
-export([signup/4, request_verification/2]).

-include("zotonic.hrl").


%% @doc Check if a module wants to redirect to the signup form.  Returns either {ok, Location} or undefined.
observe_signup_url(#signup_url{props=Props, signup_props=SignupProps}, Context) ->
    CheckId = z_ids:id(),
    z_session:set(signup_xs, {CheckId, Props, SignupProps}, Context),
    {ok, z_dispatcher:url_for(signup, [{xs, CheckId}], Context)}.


observe_identity_verification(#identity_verification{user_id=UserId, identity=undefined}, Context) ->
    request_verification(UserId, Context);
observe_identity_verification(#identity_verification{user_id=UserId, identity=Ident}, Context) ->
    case proplists:get_value(type, Ident) of
        <<"email">> -> send_verify_email(UserId, Ident, Context);
        _ -> false
    end.


%% @doc Return the url to redirect to when the user logged on, defaults to the user's personal page.
observe_logon_ready_page(#logon_ready_page{request_page=[]}, Context) ->
    case z_auth:is_auth(Context) of
        true -> m_rsc:p(z_acl:user(Context), page_url, Context);
        false -> []
    end;
observe_logon_ready_page(#logon_ready_page{request_page=Url}, _Context) ->
    Url.


%% @doc Sign up a new user.
%% @spec signup(proplist(), proplist(), RequestConfirm, Context) -> {ok, UserId} | {error, Reason}
signup(Props, SignupProps, RequestConfirm, Context) ->
    ContextSudo = z_acl:sudo(Context),
    case check_signup(Props, SignupProps, ContextSudo) of
        {ok, Props1, SignupProps1} ->
            z_db:transaction(fun(Ctx) -> do_signup(Props1, SignupProps1, RequestConfirm, Ctx) end, ContextSudo);
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
            case check_identity(SignupProps1, Context) of
                ok -> 
                    case check_props(Props1, Context) of
                        ok -> {ok, Props1, SignupProps1};
                        {error, _} = Error -> Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _ContextOrReason} = Error ->
            Error
    end.


%% @doc Preflight check if the props are ok.
%% @todo Add some checks on name, title etc.
check_props(_Props, _Context) ->
    ok.

%% @doc Preflight check on identities, prevent double identity keys.
check_identity([], _Context) ->
    ok;
check_identity([{identity, {username_pw, {Username, _Password}, true, _Verified}}|Idents], Context) ->
    case username_exists(Username, Context) of
        false -> check_identity(Idents, Context);
        true -> {error, {identity_in_use, username}}
    end;
check_identity([{identity, {Type, Key, true, _Verified}}|Idents], Context) ->
    case identity_exists(Type, Key, Context) of
        false -> check_identity(Idents, Context);
        true -> {error, {identity_in_use, Type}}
    end;
check_identity([_|Idents], Context) ->
    check_identity(Idents, Context).


%% @doc Insert a new user, return the user id on success. Assume all args are ok as we did
%% a preflight check and users sign up slowly (ie. no race condition)
%% This function is called with a 'sudo' context within a transaction. We throw errors to
%% force a rollback of the transaction.
do_signup(Props, SignupProps, RequestConfirm, Context) ->
    IsVerified = not RequestConfirm orelse has_verified_identity(SignupProps),
    case m_rsc:insert(props_to_rsc(Props, IsVerified, Context), Context) of
        {ok, Id} ->
            [ insert_identity(Id, Ident, Context) || {K,Ident} <- SignupProps, K == identity ],
            z_notifier:map(#signup_done{id=Id, is_verified=IsVerified, props=Props, signup_props=SignupProps}, Context),
            case IsVerified of
                true -> z_notifier:map(#signup_confirm{id=Id}, Context);
                false -> nop
            end,    
            {ok, Id};
        {error, Reason} ->
            throw({error, Reason})
    end.

    
    has_verified_identity([]) -> false;
    has_verified_identity([{identity, {Type, _, _, true}}|_Is]) when Type /= username_pw -> true;
    has_verified_identity([_|Is]) -> has_verified_identity(Is).


insert_identity(Id, {username_pw, {Username, Password}, true, true}, Context) ->
    case m_identity:set_username_pw(Id, Username, Password, Context) of
        ok -> ok;
        Error -> throw(Error)
    end;
insert_identity(Id, {Type, Key, true, Verified}, Context) when is_binary(Key); is_list(Key) ->
    m_identity:insert_unique(Id, Type, Key, [{is_verified, Verified}], Context);
insert_identity(Id, {Type, Key, false, Verified}, Context) when is_binary(Key); is_list(Key) ->
    m_identity:insert(Id, Type, Key, [{is_verified, Verified}], Context).


props_to_rsc(Props, IsVerified, Context) ->
    Category = z_convert:to_atom(m_config:get_value(mod_signup, member_category, person, Context)),
    VisibleFor = z_convert:to_integer(m_config:get_value(mod_signup, member_visible_for, 0, Context)),
    Props1 = [
        {is_published, IsVerified},
        {visible_for, VisibleFor},
        {category, Category},
        {is_verified_account, IsVerified},
        {creator_id, self},
        {pref_language, z_context:language(Context)}
        | Props
    ],
    case proplists:is_defined(title, Props1) of
        true -> 
            Props1;
        false ->
            Name = [
                z_convert:to_list(proplists:get_value(name_first, Props1)),
                z_convert:to_list(proplists:get_value(name_surname_prefix, Props1)),
                z_convert:to_list(proplists:get_value(name_surname, Props1))
            ],
            Name1 = lists:filter(fun(S) -> not z_utils:is_empty(S) end, Name),
            Name2 = string:join(Name1, " "),
            [ {title, Name2} | Props1 ]
    end.


%% @doc Check if a username exists
username_exists(Username, Context) ->
    case m_identity:lookup_by_username(Username, Context) of
        undefined -> false;
        _Props -> true
    end.

%% @doc Check if the identity exists
identity_exists(Type, Key, Context) ->
    case m_identity:lookup_by_type_and_key(Type, Key, Context) of
        undefined -> false;
        _Props -> true
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
    z_email:send_render(Email, "email_verify.tpl", Vars, z_acl:sudo(Context)),
    ok.


manage_schema(install, _Context) ->
    #datamodel{
        resources=[
            {signup_tos, text, [
                            {is_published, true},
                            {visible_for, 0},
                            {page_path, "/terms"},
                            {title, "Terms of Service"},
                            {summary, "These Terms of Service (“Terms”) govern your access to and use of the services and COMPANY’s web sites (the “Services”), and any information, text, graphics, or other materials uploaded, downloaded or appearing on the Services (collectively referred to as “Content”). Your access to and use of the Services is conditioned on your acceptance of and compliance with these Terms. By accessing or using the Services you agree to be bound by these Terms."},
                            {body, "<h2>INSERT YOUR TERMS OF SERVICE HERE</h2>"}
                        ]},
            {signup_privacy, text, [
                            {is_published, true},
                            {visible_for, 0},
                            {page_path, "/privacy"},
                            {title, "Privacy Policy"},
                            {summary, "This Privacy Policy describes COMPANY’s policies and procedures on the collection, use and disclosure of your information. COMPANY receives your information through our various web sites, SMS, APIs, services and third-parties (“Services”). When using any of our Services you consent to the collection, transfer, manipulation, storage, disclosure and other uses of your information as described in this Privacy Policy. Irrespective of which country that you reside in or create information from, your information may be used by COMPANY in any country where COMPANY operates."},
                            {body, "<h2>INSERT YOUR PRIVACY POLICY HERE</h2>"}
                        ]}
        ]
    }.
