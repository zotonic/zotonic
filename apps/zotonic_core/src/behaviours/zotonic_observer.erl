%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Observer behaviour for all notifications. Use this behaviour
%% in your module file to allow type checking of your observers.
%% @end

%% Copyright 2025 Marc Worrell
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

-module(zotonic_observer).
-author("Marc Worrell <marc@worrell.nl").

-include_lib("zotonic.hrl").

%% Try to find the site for the request
%% Called when the request Host doesn't match any active site.
%% Type: first
-callback observe_dispatch_host(#dispatch_host{}, z:context()) -> {ok, #dispatch_redirect{}} | undefined.
-callback pid_observe_dispatch_host(pid(), #dispatch_host{}, z:context()) -> {ok, #dispatch_redirect{}} | undefined.

-optional_callbacks([ observe_dispatch_host/2, pid_observe_dispatch_host/3 ]).

%% Final try for dispatch, try to match the request.
%% Called when the site is known, but no match is found for the path
%% Type: first
-callback observe_dispatch(#dispatch{}, z:context()) -> {ok, Result} | undefined when
    Result :: m_rsc:resource_id()
            | #dispatch_match{}
            | #dispatch_redirect{}.
-callback pid_observe_dispatch(pid(), #dispatch{}, z:context()) -> {ok, Result} | undefined when
    Result :: m_rsc:resource_id()
            | #dispatch_match{}
            | #dispatch_redirect{}.

-optional_callbacks([ observe_dispatch/2, pid_observe_dispatch/3 ]).

%% Check and possibly modify the CSP response security headers
%% Accumulator is the modified CSP headers, notification is the default
%% set of CSP headers as provided by the Zotonic core routines.
%% Type: foldr
-callback observe_content_security_header(Default, Acc, Context) -> Result when
    Default :: #content_security_header{},
    Acc :: #content_security_header{},
    Context :: z:context(),
    Result :: #content_security_header{}.
-callback pid_observe_content_security_header(pid(), Default, Acc, Context) -> Result when
    Default :: #content_security_header{},
    Acc :: #content_security_header{},
    Context :: z:context(),
    Result :: #content_security_header{}.

-optional_callbacks([ observe_content_security_header/3, pid_observe_content_security_header/4 ]).

%% Check and possibly modify the http response security headers
%% All headers are in lowercase.
%% Type: first
-callback observe_security_headers(#security_headers{}, z:context()) -> Result when
    Result :: #security_headers{} | undefined.
-callback pid_observe_security_headers(pid(), #security_headers{}, z:context()) -> Result when
    Result :: #security_headers{} | undefined.

-optional_callbacks([ observe_security_headers/2, pid_observe_security_headers/3 ]).

%% Set CORS headers on the HTTP response.
%% Type: first
-callback observe_cors_headers(#cors_headers{}, z:context()) -> Result when
    Result :: #cors_headers{} | undefined.
-callback pid_observe_cors_headers(pid(), #cors_headers{}, z:context()) -> Result when
    Result :: #cors_headers{} | undefined.

-optional_callbacks([ observe_cors_headers/2, pid_observe_cors_headers/3 ]).

%% Let all modules add resource specific response headers to the request.
%% The accumulator is the list of headers to be set.
%% Type: foldl
-callback observe_resource_headers(#resource_headers{}, Acc, Context) -> Result when
    Acc :: [ {binary(), binary()} ],
    Context :: z:context(),
    Result :: [ {binary(), binary()} ].

-optional_callbacks([ observe_resource_headers/3 ]).

%% Access log event for http. Called from the z_stats.
%% Type: notify_sync
-callback observe_http_log_access(#http_log_access{}, z:context()) -> any().
-callback pid_observe_http_log_access(pid(), #http_log_access{}, z:context()) -> any().

-optional_callbacks([ observe_http_log_access/2, pid_observe_http_log_access/3 ]).

%% 'module_ready' - Sent when modules have changed, z_module_indexer reindexes all modules' templates, actions etc.
%% Type: notify_sync
-callback observe_module_ready(module_ready, z:context()) -> any().
-callback pid_observe_module_ready(pid(), module_ready, z:context()) -> any().

-optional_callbacks([ observe_module_ready/2, pid_observe_module_ready/3 ]).

%% A module has been activated and started.
%% Type: notify
-callback observe_module_activate(#module_activate{}, z:context()) -> any().
-callback pid_observe_module_activate(pid(), #module_activate{}, z:context()) -> any().

-optional_callbacks([ observe_module_activate/2, pid_observe_module_activate/3 ]).

%% A module has been stopped and deactivated.
%% Type: notify
-callback observe_module_deactivate(#module_deactivate{}, z:context()) -> any().
-callback pid_observe_module_deactivate(pid(), #module_deactivate{}, z:context()) -> any().

-optional_callbacks([ observe_module_deactivate/2, pid_observe_module_deactivate/3 ]).

%% Get available content types and their dispatch rules
%% Example: {{<<"text">>, <<"html">>, []}, page}
%% A special dispatch rule is 'page_url', which refers to the page_url property of the resource.
%% Type: foldr
-callback observe_content_types_dispatch(#content_types_dispatch{}, Acc, Context) -> Result when
    Acc :: [ {ContentType, atom()} ],
    Context :: z:context(),
    Result :: [ {ContentType, atom()} ],
    ContentType :: {binary(), binary(), list()}.
-callback pid_observe_content_types_dispatch(pid(), #content_types_dispatch{}, Acc, Context) -> Result when
    Acc :: [ {ContentType, atom()} ],
    Context :: z:context(),
    Result :: [ {ContentType, atom()} ],
    ContentType :: {binary(), binary(), list()}.

-optional_callbacks([ observe_content_types_dispatch/3, pid_observe_content_types_dispatch/4 ]).


%% Check where to go after a user logs on.
%% Type: first
%% Return: a URL or ``undefined``
-callback observe_logon_ready_page(#logon_ready_page{}, z:context()) -> Result when
    Result :: binary() | undefined.
-callback pid_observe_logon_ready_page(pid(), #logon_ready_page{}, z:context()) -> Result when
    Result :: binary() | undefined.

-optional_callbacks([ observe_logon_ready_page/2, pid_observe_logon_ready_page/3 ]).

%% Handle a user logon. The posted query args are included.
%% Type: first
-callback observe_logon_submit(#logon_submit{}, z:context()) -> Result when
    Result :: {ok, UserId :: m_rsc:resource_id()}
            | {error, term()}
            | undefined.
-callback pid_observe_logon_submit(pid(), #logon_submit{}, z:context()) -> Result when
    Result :: {ok, UserId :: m_rsc:resource_id()}
            | {error, term()}
            | undefined.

-optional_callbacks([ observe_logon_submit/2, pid_observe_logon_submit/3 ]).

%% Check for logon options, called if logon_submit returns `undefined`.
%% This is used to fetch external (or local) authentication links for an
%% username.
%% Type: foldl
-callback observe_logon_options(#logon_options{}, Acc, Context) -> Result when
    Acc :: map(),
    Context :: z:context(),
    Result :: map().
-callback pid_observe_logon_options(pid(), #logon_options{}, Acc, Context) -> Result when
    Acc :: map(),
    Context :: z:context(),
    Result :: map().

-optional_callbacks([ observe_logon_options/3, pid_observe_logon_options/4 ]).

%% Request to send a verification to the user. Return ok or an error.
%% Handled by mod_signup to send out verification emails.
%% Type: first
%% Identity may be undefined, or is an identity used for the verification.
-callback observe_identity_verification(#identity_verification{}, z:context()) -> Result when
    Result :: ok
            | {error, term()}
            | undefined.
-callback pid_observe_identity_verification(pid(), #identity_verification{}, z:context()) -> Result when
    Result :: ok
            | {error, term()}
            | undefined.

-optional_callbacks([ observe_identity_verification/2, pid_observe_identity_verification/3 ]).

%% Notify that a user's identity has been verified. Signals to modules
%% handling identities to mark this identity as verified. Handled by mod_admin_identity
%% to call the m_identity model for this type/key.
%% Type: notify
-callback observe_identity_verified(#identity_verified{}, z:context()) -> any().
-callback pid_observe_identity_verified(pid(), #identity_verified{}, z:context()) -> any().

-optional_callbacks([ observe_identity_verified/2, pid_observe_identity_verified/3 ]).

%% Check if passwords are matching. Uses the password hashing algorithms.
%% Type: first
-callback observe_identity_password_match(#identity_password_match{}, z:context()) -> undefined | boolean().
-callback pid_observe_identity_password_match(pid(), #identity_password_match{}, z:context()) -> undefined | boolean().

-optional_callbacks([ observe_identity_password_match/2, pid_observe_identity_password_match/3 ]).

%% Notify that a user's identity has been updated by the identity model.
%% Type: notify
-callback observe_identity_update_done(#identity_update_done{}, z:context()) -> any().
-callback pid_observe_identity_update_done(pid(), #identity_update_done{}, z:context()) -> any().

-optional_callbacks([ observe_identity_update_done/2, pid_observe_identity_update_done/3 ]).

%% Handle a signup of a user, return the follow on page for after the signup.
%% Type: first
%% 'props' is a map with properties for the person resource (email, name, etc)
%% 'signup_props' is a proplist with 'identity' definitions and optional follow on url 'ready_page'
%% An identity definition is {Kind, Identifier, IsUnique, IsVerified}
-callback observe_signup_url(#signup_url{}, z:context()) -> Result when
    Result :: {ok, Url :: binary()}
            | undefined.
-callback pid_observe_signup_url(pid(), #signup_url{}, z:context()) -> Result when
    Result :: {ok, Url :: binary()}
            | undefined.

-optional_callbacks([ observe_signup_url/2, pid_observe_signup_url/3 ]).

%% Request a signup of a new or existing user. Arguments are similar to #signup_url{}
%% Type: first
%% Returns {ok, UserId} or {error, Reason}
-callback observe_signup(#signup{}, z:context()) -> Result when
    Result :: {ok, UserId :: m_rsc:resource_id()}
            | {error, term()}
            | undefined.
-callback pid_observe_signup(pid(), #signup{}, z:context()) -> Result when
    Result :: {ok, UserId :: m_rsc:resource_id()}
            | {error, term()}
            | undefined.

-optional_callbacks([ observe_signup/2, pid_observe_signup/3 ]).

%% Signup failed, give the error page URL. Return {ok, Url} or undefined.
%% Reason is returned by the signup handler for the particular signup method (username, facebook etc)
%% Type: first
-callback observe_signup_failed_url(#signup_failed_url{}, z:context()) -> Result when
    Result :: {ok, Url :: binary()}
            | undefined.
-callback pid_observe_signup_failed_url(pid(), #signup_failed_url{}, z:context()) -> Result when
    Result :: {ok, Url :: binary()}
            | undefined.

-optional_callbacks([ observe_signup_failed_url/2, pid_observe_signup_failed_url/3 ]).

%% signup_check
%% Check if the signup can be handled, a fold over all modules.
%% Fold argument/result is {ok, Props, SignupProps} or {error, Reason}
%% Type: foldl
%% Return: ``{ok, Props, SignupProps}`` or ``{error, Reason}``
-callback observe_signup_check(signup_check, Acc, Context) -> Result when
    Acc :: {ok, Props, SignupProps} | {error, term()},
    Context :: z:context(),
    Result :: {ok, Props, SignupProps} | {error, term()},
    Props :: map(),
    SignupProps :: list().
-callback pid_observe_signup_check(pid(), signup_check, Acc, Context) -> Result when
    Acc :: {ok, Props, SignupProps} | {error, term()},
    Context :: z:context(),
    Result :: {ok, Props, SignupProps} | {error, term()},
    Props :: map(),
    SignupProps :: list().

-optional_callbacks([ observe_signup_check/3, pid_observe_signup_check/4 ]).


%% Signal that a user has been signed up (map, result is ignored)
%% Type: notify_sync
-callback observe_signup_done(#signup_done{}, z:context()) -> any().
-callback pid_observe_signup_done(pid(), #signup_done{}, z:context()) -> any().

-optional_callbacks([ observe_signup_done/2, pid_observe_signup_done/3 ]).


%% Signal that a user has been confirmed. (map, result is ignored)
%% Type: notify
-callback observe_signup_confirm(#signup_confirm{}, z:context()) -> any().
-callback pid_observe_signup_confirm(pid(), #signup_confirm{}, z:context()) -> any().

-optional_callbacks([ observe_signup_confirm/2, pid_observe_signup_confirm/3 ]).

%% Fetch the page a user is redirected to after signing up with a confirmed identity
%% Type: first
%% Return: a URL or ``undefined``
-callback observe_signup_confirm_redirect(#signup_confirm_redirect{}, z:context()) -> Result when
    Result :: URL
            | undefined,
    URL :: binary().
-callback pid_observe_signup_confirm_redirect(pid(), #signup_confirm_redirect{}, z:context()) -> Result when
    Result :: URL
            | undefined,
    URL :: binary().

-optional_callbacks([ observe_signup_confirm_redirect/2, pid_observe_signup_confirm_redirect/3 ]).

%% Notification to signal an inserted comment.
%% 'comment_id' is the id of the inserted comment, 'id' is the id of the resource commented on.
%% Type: notify
-callback observe_comment_insert(#comment_insert{}, z:context()) -> any().
-callback pid_observe_comment_insert(pid(), #comment_insert{}, z:context()) -> any().

-optional_callbacks([ observe_comment_insert/2, pid_observe_comment_insert/3 ]).

%% Notify that the session's language has been changed
%% Type: notify
-callback observe_language(#language{}, z:context()) -> any().
-callback pid_observe_language(pid(), #language{}, z:context()) -> any().

-optional_callbacks([ observe_language/2, pid_observe_language/3 ]).

%% Set the language of the context to a user's prefered language
%% Type: first
-callback observe_set_user_language(#set_user_language{}, z:context()) -> z:context() | undefined.
-callback pid_observe_set_user_language(pid(), #set_user_language{}, z:context()) -> z:context() | undefined.

-optional_callbacks([ observe_set_user_language/2, pid_observe_set_user_language/3 ]).

%% Make a generated URL absolute, optionally called after url_rewrite by z_dispatcher
%% Type: first
-callback observe_url_abs(#url_abs{}, z:context()) -> URL | undefined when
    URL :: binary().
-callback pid_observe_url_abs(pid(), #url_abs{}, z:context()) -> URL | undefined when
    URL :: binary().

-optional_callbacks([ observe_url_abs/2, pid_observe_url_abs/3 ]).

%% Rewrite a URL after it has been generated using the z_dispatcher
%% Type: foldl
-callback observe_url_rewrite(#url_rewrite{}, Acc, z:context()) -> Result when
    Acc :: binary(),
    Result :: binary().
-callback pid_observe_url_rewrite(pid(), #url_rewrite{}, Acc, z:context()) -> Result when
    Acc :: binary(),
    Result :: binary().

-optional_callbacks([ observe_url_rewrite/3, pid_observe_url_rewrite/4 ]).

%% Rewrite a URL before it will be dispatched using the z_sites_dispatcher
%% Type: foldl
-callback observe_dispatch_rewrite(#dispatch_rewrite{}, Acc, z:context()) -> Result when
    Acc :: binary(),
    Result :: binary().
-callback pid_observe_dispatch_rewrite(pid(), #dispatch_rewrite{}, Acc, z:context()) -> Result when
    Acc :: binary(),
    Result :: binary().

-optional_callbacks([ observe_dispatch_rewrite/3, pid_observe_dispatch_rewrite/4 ]).

%% Request the SSL certificates for this site. The server_name property contains the hostname used by the client.
%% Type: first
%% Returns either 'undefined' or a list of ssl options (type ssl:ssl_option())
-callback observe_ssl_options(#ssl_options{}, z:context()) -> SSLOptions | undefined when
    SSLOptions :: {ok, list( ssl:tls_option() )}.
-callback pid_observe_ssl_options(pid(), #ssl_options{}, z:context()) -> SSLOptions | undefined when
    SSLOptions :: {ok, list( ssl:tls_option() )}.

-optional_callbacks([ observe_ssl_options/2, pid_observe_ssl_options/3 ]).

%% Used in the admin to fetch the possible blocks for display
%% Type: foldl
-callback observe_admin_edit_blocks(#admin_edit_blocks{}, Acc, z:context()) -> Result when
    Acc :: BlockGroups,
    Result :: BlockGroups,
    BlockGroups :: [ {Prio, SectionTitle, BlockTypes} ],
    Prio :: integer(),
    SectionTitle :: binary() | string() | z:trans(),
    BlockTypes :: [ {atom(), binary() | string() | z:trans()}].
-callback pid_observe_admin_edit_blocks(pid(), #admin_edit_blocks{}, Acc, z:context()) -> Result when
    Acc :: BlockGroups,
    Result :: BlockGroups,
    BlockGroups :: [ {Prio, SectionTitle, BlockTypes} ],
    Prio :: integer(),
    SectionTitle :: binary() | string() | z:trans(),
    BlockTypes :: [ {atom(), binary() | string() | z:trans()}].

-optional_callbacks([ observe_admin_edit_blocks/3, pid_observe_admin_edit_blocks/4 ]).

%% Used in the admin to process a submitted resource form's query args.
%% Type: foldl
-callback observe_admin_rscform(#admin_rscform{}, Acc, z:context()) -> Result when
    Acc :: Props,
    Result :: Props,
    Props :: [ {binary(), z:qvalue()} ].
-callback pid_observe_admin_rscform(pid(), #admin_rscform{}, Acc, z:context()) -> Result when
    Acc :: Props,
    Result :: Props,
    Props :: [ {binary(), z:qvalue()} ].

-optional_callbacks([ observe_admin_rscform/3, pid_observe_admin_rscform/4 ]).

%% Used for fetching the menu in the admin. The menu items are expected to be of the
%% type #menu_item{}, as defined in mod_menu.hrl
%% Type: foldl
%% Return: list of admin menu items
-callback observe_admin_menu(#admin_menu{}, Acc, z:context()) -> Result when
    Acc :: MenuItems,
    Result :: MenuItems,
    MenuItems :: [ term() ].
-callback pid_observe_admin_menu(pid(), #admin_menu{}, Acc, z:context()) -> Result when
    Acc :: MenuItems,
    Result :: MenuItems,
    MenuItems :: [ term() ].

-optional_callbacks([ observe_admin_menu/3, pid_observe_admin_menu/4 ]).

%% Fetch the menu id belonging to a certain resource.
%% Type: first
-callback observe_menu_rsc(#menu_rsc{}, z:context()) -> m_rsc:resource() | undefined.
-callback pid_observe_menu_rsc(pid(), #menu_rsc{}, z:context()) -> m_rsc:resource() | undefined.

-optional_callbacks([ observe_menu_rsc/2, pid_observe_menu_rsc/3 ]).

%% Fold for mapping non-iolist output to iolist values.
%% Used when outputting a rendered HTML tree.
%% Folded accumulator is: { MixedHtml, Context }
%% Type: foldl
-callback observe_output_html(#output_html{}, Acc, z:context()) -> Result when
    Acc :: {MixedHtml, Context},
    Result :: {MixedHtml, Context},
    Context :: z:context(),
    MixedHtml :: binary() | list().
-callback pid_observe_output_html(pid(), #output_html{}, Acc, z:context()) -> Result when
    Acc :: {MixedHtml, Context},
    Result :: {MixedHtml, Context},
    Context :: z:context(),
    MixedHtml :: binary() | list().

-optional_callbacks([ observe_output_html/3, pid_observe_output_html/4 ]).

%% An activity in Zotonic. When this is handled as a notification then return a list
%% of patterns matching this activity.  These patterns are then used to find interested
%% subscribers.
%% Type: map
-callback observe_activity(#activity{}, z:context()) -> Patterns when
    Patterns :: [ term() ].
-callback pid_observe_activity(pid(), #activity{}, z:context()) -> Patterns when
    Patterns :: [ term() ].

-optional_callbacks([ observe_activity/2, pid_observe_activity/3 ]).

%% Push a list of activities via a 'channel' (eg 'email') to a recipient.
%% The activities are a list of #activity{} records.
%% Type: first
-callback observe_activity_send(#activity_send{}, z:context()) -> undefined | ok.
-callback pid_observe_activity_send(pid(), #activity_send{}, z:context()) -> undefined | ok.

-optional_callbacks([ observe_activity_send/2, pid_observe_activity_send/3 ]).

%% Notification sent to a site when e-mail for that site is received
%% Type: first
-callback observe_email_received(#email_received{}, z:context()) -> Result when
    Result :: undefined
            | {ok, MsgId :: binary()}
            | {ok, term()}
            | ok
            | {error, term()}
            | undefined.
-callback pid_observe_email_received(pid(), #email_received{}, z:context()) -> Result when
    Result :: undefined
            | {ok, MsgId :: binary()}
            | {ok, term()}
            | ok
            | {error, term()}
            | undefined.

-optional_callbacks([ observe_email_received/2, pid_observe_email_received/3 ]).

% % E-mail received notification:
% % {z_convert:to_atom(Notification), received, UserId, ResourceId, Received}
% % The {Notification, UserId, ResourceId} comes from m_email_receive_recipient:get_by_recipient/2.


%% Check if an email address is blocked
%% Type: first
-callback observe_email_is_blocked(#email_is_blocked{}, z:context()) -> boolean() | undefined.
-callback pid_observe_email_is_blocked(pid(), #email_is_blocked{}, z:context()) -> boolean() | undefined.

-optional_callbacks([ observe_email_is_blocked/2, pid_observe_email_is_blocked/3 ]).

%% @doc Check if an email address is safe to send email to. The email address is not blocked
%% and is not marked as bouncing.
%% Type: first
-callback observe_email_is_recipient_ok(#email_is_recipient_ok{}, z:context()) -> boolean() | undefined.
-callback pid_observe_email_is_recipient_ok(pid(), #email_is_recipient_ok{}, z:context()) -> boolean() | undefined.

-optional_callbacks([ observe_email_is_recipient_ok/2, pid_observe_email_is_recipient_ok/3 ]).

%% Email status notification, sent when the validity of an email recipient changes
%% Type: notify
-callback observe_email_status(#email_status{}, z:context()) -> any().
-callback pid_observe_email_status(pid(), #email_status{}, z:context()) -> any().

-optional_callbacks([ observe_email_status/2, pid_observe_email_status/3 ]).

%% Bounced e-mail notification.  The recipient is the e-mail that is bouncing. When the
%% the message_nr is unknown the it is set to 'undefined'. This can happen if it is a "late bounce".
%% If the recipient is defined then the Context is the depickled z_email:send/2 context.
%% Type: notify
-callback observe_email_bounced(#email_bounced{}, z:context()) -> any().
-callback pid_observe_email_bounced(pid(), #email_bounced{}, z:context()) -> any().

-optional_callbacks([ observe_email_bounced/2, pid_observe_email_bounced/3 ]).

%% Notify that we could send an e-mail (there might be a bounce later...)
%% The Context is the depickled z_email:send/2 context.
%% Type: notify
-callback observe_email_sent(#email_sent{}, z:context()) -> any().
-callback pid_observe_email_sent(pid(), #email_sent{}, z:context()) -> any().

-optional_callbacks([ observe_email_sent/2, pid_observe_email_sent/3 ]).

%% Notify that we could NOT send an e-mail (there might be a bounce later...)
%% The Context is the depickled z_email:send/2 context.
%% Type: notify
-callback observe_email_failed(#email_failed{}, z:context()) -> any().
-callback pid_observe_email_failed(pid(), #email_failed{}, z:context()) -> any().

-optional_callbacks([ observe_email_failed/2, pid_observe_email_failed/3 ]).

%% Return the options for the DKIM signature on outgoing emails. Called during
%% email encoding.
%% Type: first
%% Return: ``list()`` options for the DKIM signature
-callback observe_email_dkim_options(#email_dkim_options{}, z:context()) -> Result when
    Result :: DKIMOptions
            | undefined,
    DKIMOptions :: list( {atom(), term()} ).
-callback pid_observe_email_dkim_options(pid(), #email_dkim_options{}, z:context()) -> Result when
    Result :: DKIMOptions
            | undefined,
    DKIMOptions :: list( {atom(), term()} ).

-optional_callbacks([ observe_email_dkim_options/2, pid_observe_email_dkim_options/3 ]).

%% Request to send an email using special email senders, for example using
%% proxy APIs. If no sender is found then the email is sent using the built-in smtp
%% server. The email is completely mime encoded.
%% The Context is the depickled z_email:send/2 context.
%% Type: first
%% Return: ``{ok, Status}`` where status is a binary; or
%%         ``smtp`` use the built-in smtp server; or
%%         ``{error, Reason::atom(), {FailureType, Host, Message}}`` when FailureType
%%         is one of ``permanent_failure`` or ``temporary_failure``.
-callback observe_email_send_encoded(#email_send_encoded{}, z:context()) -> Result when
    Result :: {ok, binary()}
            | {ok, term()}
            | {error, Reason, {FailureType, Host, Message}}
            | {error, term()}
            | smtp
            | undefined,
    Reason :: term(),
    FailureType :: permanent_failure | temporary_failure,
    Host :: string() | binary(),
    Message :: term().
-callback pid_observe_email_send_encoded(pid(), #email_send_encoded{}, z:context()) -> Result when
    Result :: {ok, binary()}
            | {ok, term()}
            | {error, Reason, {FailureType, Host, Message}}
            | {error, term()}
            | smtp
            | undefined,
    Reason :: term(),
    FailureType :: permanent_failure | temporary_failure,
    Host :: string() | binary(),
    Message :: term().

-optional_callbacks([ observe_email_send_encoded/2, pid_observe_email_send_encoded/3 ]).

%% Add a handler for receiving e-mail notifications
%% Type: first
%% Return: ``{ok, LocalFrom}``, the unique localpart of an e-mail address on this server.
-callback observe_email_add_handler(#email_add_handler{}, z:context()) -> Result when
    Result :: {ok, LocalForm :: binary()}
            | undefined.
-callback pid_observe_email_add_handler(pid(), #email_add_handler{}, z:context()) -> Result when
    Result :: {ok, LocalForm :: binary()}
            | undefined.

-optional_callbacks([ observe_email_add_handler/2, pid_observe_email_add_handler/3 ]).

-callback observe_email_ensure_handler(#email_ensure_handler{}, z:context()) -> Result when
    Result :: {ok, LocalForm :: binary()}
            | undefined.
-callback pid_observe_email_ensure_handler(pid(), #email_ensure_handler{}, z:context()) -> Result when
    Result :: {ok, LocalForm :: binary()}
            | undefined.

-optional_callbacks([ observe_email_ensure_handler/2, pid_observe_email_ensure_handler/3 ]).

%% Drop an e-mail handler for a user/resource id. (notify).
%% The notification, user and resource should be the same as when the handler was registered.
-callback observe_email_drop_handler(#email_drop_handler{}, z:context()) -> any().
-callback pid_observe_email_drop_handler(pid(), #email_drop_handler{}, z:context()) -> any().

-optional_callbacks([ observe_email_drop_handler/2, pid_observe_email_drop_handler/3 ]).

%% Send a page to a mailinglist (notify)
%% Use {single_test_address, Email} when sending to a specific e-mail address.
-callback observe_mailinglist_mailing(#mailinglist_mailing{}, z:context()) -> any().
-callback pid_observe_mailinglist_mailing(pid(), #mailinglist_mailing{}, z:context()) -> any().

-optional_callbacks([ observe_mailinglist_mailing/2, pid_observe_mailinglist_mailing/3 ]).


%% Send a welcome or goodbye message to the given recipient.
%% The recipient is either a recipient-id or a recipient props.
%% 'what' is send_welcome, send_confirm, send_goobye or silent.
%% Type: notify
-callback observe_mailinglist_message(#mailinglist_message{}, z:context()) -> any().
-callback pid_observe_mailinglist_message(pid(), #mailinglist_message{}, z:context()) -> any().

-optional_callbacks([ observe_mailinglist_message/2, pid_observe_mailinglist_message/3 ]).

%% Save (and update) the complete category hierarchy
%% Type: notify
-callback observe_category_hierarchy_save(#category_hierarchy_save{}, z:context()) -> any().
-callback pid_observe_category_hierarchy_save(pid(), #category_hierarchy_save{}, z:context()) -> any().

-optional_callbacks([ observe_category_hierarchy_save/2, pid_observe_category_hierarchy_save/3 ]).

%% Save the menu tree of a menu resource
%% Type: notify
-callback observe_menu_save(#menu_save{}, z:context()) -> any().
-callback pid_observe_menu_save(pid(), #menu_save{}, z:context()) -> any().

-optional_callbacks([ observe_menu_save/2, pid_observe_menu_save/3 ]).

%% Signal that the hierarchy underneath a resource has been changed by mod_menu
%% Type: notify
-callback observe_hierarchy_updated(#hierarchy_updated{}, z:context()) -> any().
-callback pid_observe_hierarchy_updated(pid(), #hierarchy_updated{}, z:context()) -> any().

-optional_callbacks([ observe_hierarchy_updated/2, pid_observe_hierarchy_updated/3 ]).

%% Resource is read, opportunity to add computed fields
%% Used in a foldr with the read properties as accumulator.
%% Type: foldr
-callback observe_rsc_get(#rsc_get{}, Acc, z:context()) -> Result when
    Acc :: m_rsc:props(),
    Result :: map().
-callback pid_observe_rsc_get(pid(), #rsc_delete{}, Acc, z:context()) -> Result when
    Acc :: m_rsc:props(),
    Result :: m_rsc:props().

-optional_callbacks([ observe_rsc_get/3, pid_observe_rsc_get/4 ]).

%% Resource will be deleted.
%% This notification is part of the delete transaction, it's purpose is to clean up
%% associated data.
%% Type: notify
-callback observe_rsc_delete(#rsc_delete{}, z:context()) -> any().
-callback pid_observe_rsc_delete(pid(), #rsc_delete{}, z:context()) -> any().

-optional_callbacks([ observe_rsc_delete/2, pid_observe_rsc_delete/3 ]).

%% Foldr for an resource insert, these are the initial properties and will overrule
%% the properties in the insert request. Use with care.  The props are the properties of
%% the later insert, after escaping/filtering but before the #rsc_update{} notification below.
%% Type: foldr
-callback observe_rsc_insert(#rsc_insert{}, Acc, z:context()) -> Result when
    Acc :: m_rsc:props(),
    Result :: m_rsc:props().
-callback pid_observe_rsc_insert(pid(), #rsc_insert{}, Acc, z:context()) -> Result when
    Acc :: m_rsc:props(),
    Result :: m_rsc:props().

-optional_callbacks([ observe_rsc_insert/3, pid_observe_rsc_insert/4 ]).

%% Map to signal merging two resources. Move any information from the loser to the
%% winner. The loser will be deleted.
%% Type: notify_sync
-callback observe_rsc_merge(#rsc_merge{}, z:context()) -> any().
-callback pid_observe_rsc_merge(pid(), #rsc_merge{}, z:context()) -> any().

-optional_callbacks([ observe_rsc_merge/2, pid_observe_rsc_merge/3 ]).

%% An updated resource is about to be persisted.
%% Observe this notification to change the resource properties before they are
%% persisted.
%% The props are the resource's props _before_ the update, but _after_ filtering
%% and sanitization. The folded value is ``{ok, UpdateProps}`` for the update itself.
%% Type: foldr
%% Return: ``{ok, UpdateProps}`` or ``{error, term()}``
-callback observe_rsc_update(#rsc_update{}, Acc, z:context()) -> Result when
    Acc :: {ok, map()} | {error, term()},
    Result :: {ok, map()} | {error, term()}.
-callback pid_observe_rsc_update(pid(), #rsc_update{}, Acc, z:context()) -> Result when
    Acc :: {ok, map()} | {error, term()},
    Result :: {ok, map()} | {error, term()}.

-optional_callbacks([ observe_rsc_update/3, pid_observe_rsc_update/4 ]).

%% An updated resource has just been persisted. Observe this notification to
%% execute follow-up actions for a resource update.
%% Type: notify
%% Return: return value is ignored
-callback observe_rsc_update_done(#rsc_update_done{}, z:context()) -> any().
-callback pid_observe_rsc_update_done(pid(), #rsc_update_done{}, z:context()) -> any().

-optional_callbacks([ observe_rsc_update_done/2, pid_observe_rsc_update_done/3 ]).

%% Upload and replace the resource with the given data. The data is in the given format.
%% Type: first
%% Return: {ok, Id} or {error, Reason}, return {error, badarg} when the data is corrupt.
-callback observe_rsc_upload(#rsc_upload{}, z:context()) -> Result when
    Result :: {ok, m_rsc:resource_id()}
            | {error, badarg | term()}
            | undefined.
-callback pid_observe_rsc_upload(pid(), #rsc_upload{}, z:context()) -> Result when
    Result :: {ok, m_rsc:resource_id()}
            | {error, badarg | term()}
            | undefined.

-optional_callbacks([ observe_rsc_upload/2, pid_observe_rsc_upload/3 ]).

%% Add custom pivot fields to a resource's search index (map)
%% Result is a single tuple or list of tuples ``{pivotname, props}``, where "pivotname"
%% is the pivot defined in a call to ``z_pivot_rsc:define_custom_pivot/3`` or a table
%% with created using a SQL command during (eg.) in a module ``manage_schema/2`` call.
%% The name of the table is ``pivot_<pivotname>``.  The ``props`` is either a property
%% list or a map with column/value pairs.
%%
%% The table MUST have an ``id`` column, with a foreign key constraint to the ``rsc``
%% table. If you define the pivot table using ``z_pivot_rsc:define_custom_pivot/3`` then
%% this column and foreign key constraint are automatically added.
%% Type: map
-callback observe_custom_pivot(#custom_pivot{}, z:context()) -> Pivots when
    Pivots :: [ Pivot ]
            | Pivot
            | ok
            | none
            | undefined,
    Pivot :: {atom(), PivotFields},
    PivotFields :: proplists:proplist()
                 | map().
-callback pid_observe_custom_pivot(pid(), #custom_pivot{}, z:context()) -> Pivots when
    Pivots :: [ Pivot ]
            | Pivot
            | ok
            | none
            | undefined,
    Pivot :: {atom(), PivotFields},
    PivotFields :: proplists:proplist()
                 | map().

-optional_callbacks([ observe_custom_pivot/2, pid_observe_custom_pivot/3 ]).

%% Fold over the resource props map to extend/remove data to be pivoted
%% Type: foldl
-callback observe_pivot_rsc_data(#pivot_rsc_data{}, Acc, z:context()) -> Result when
    Acc :: m_rsc:props(),
    Result :: m_rsc:props().
-callback pid_observe_pivot_rsc_data(pid(), #pivot_rsc_data{}, Acc, z:context()) -> Result when
    Acc :: m_rsc:props(),
    Result :: m_rsc:props().

-optional_callbacks([ observe_pivot_rsc_data/3, pid_observe_pivot_rsc_data/4 ]).

%% Pivot just before a m_rsc_update update. Used to pivot fields before the pivot itself.
%% Type: foldr
-callback observe_pivot_update(#pivot_update{}, Acc, z:context()) -> Result when
    Acc :: m_rsc:props(),
    Result :: m_rsc:props().
-callback pid_observe_pivot_update(pid(), #pivot_update{}, Acc, z:context()) -> Result when
    Acc :: m_rsc:props(),
    Result :: m_rsc:props().

-optional_callbacks([ observe_pivot_update/3, pid_observe_pivot_update/4 ]).

%% Foldr to change or add pivot fields for the main pivot table.
%% The rsc contains all rsc properties for this resource, including pivot properties.
%% Fold with a map containing the pivot fields.
%% Type: foldl
-callback observe_pivot_fields(#pivot_fields{}, Acc, z:context()) -> Result when
    Acc :: #{ binary() => term() },
    Result :: #{ binary() => term() }.
-callback pid_observe_pivot_fields(pid(), #pivot_fields{}, Acc, z:context()) -> Result when
    Acc :: #{ binary() => term() },
    Result :: #{ binary() => term() }.

-optional_callbacks([ observe_pivot_fields/3, pid_observe_pivot_fields/4 ]).

%% Signal that a resource pivot has been done.
%% Type: notify
-callback observe_rsc_pivot_done(#rsc_pivot_done{}, z:context()) -> any().
-callback pid_observe_rsc_pivot_done(pid(), #rsc_pivot_done{}, z:context()) -> any().

-optional_callbacks([ observe_rsc_pivot_done/2, pid_observe_rsc_pivot_done/3 ]).

%% Sanitize an HTML element.
%% Type: foldl
-callback observe_sanitize_element(#sanitize_element{}, Acc, z:context()) -> Result when
    Acc :: Element,
    Result :: Element,
    Element :: {binary(), list( {binary(), binary()} ), list()}.
-callback pid_observe_sanitize_element(pid(), #sanitize_element{}, Acc, z:context()) -> Result when
    Acc :: Element,
    Result :: Element,
    Element :: {binary(), list( {binary(), binary()} ), list()}.

-optional_callbacks([ observe_sanitize_element/3, pid_observe_sanitize_element/4 ]).

%% Sanitize an embed url. The hostpart is of the format: ``<<"youtube.com/v...">>``.
%% Type: first
%% Return: ``undefined``, ``false`` or a binary with a acceptable hostpath
-callback observe_sanitize_embed_url(#sanitize_embed_url{}, z:context()) -> URL when
    URL :: undefined
         | binary().
-callback pid_observe_sanitize_embed_url(pid(), #sanitize_embed_url{}, z:context()) -> URL when
    URL :: undefined
         | binary().

-optional_callbacks([ observe_sanitize_embed_url/2, pid_observe_sanitize_embed_url/3 ]).

%% Check if a user is the owner of a resource.
%% ``id`` is the resource id.
%% Type: first
-callback observe_acl_is_owner(#acl_is_owner{}, z:context()) -> boolean() | undefined.
-callback pid_observe_acl_is_owner(pid(), #acl_is_owner{}, z:context()) -> boolean() | undefined.

-optional_callbacks([ observe_acl_is_owner/2, pid_observe_acl_is_owner/3 ]).

%% Check if a user is authorized to perform an operation on a an object
%% (some resource or module). Observe this notification to do complex or more
%% fine-grained authorization checks than you can do through the ACL rules admin
%% interface. Defaults to ``false``.
%% Type: first
%% Return: ``true`` to allow the operation, ``false`` to deny it or ``undefined`` to let the next observer decide
-callback observe_acl_is_allowed(#acl_is_allowed{}, z:context()) -> boolean() | undefined.
-callback pid_observe_acl_is_allowed(pid(), #acl_is_allowed{}, z:context()) -> boolean() | undefined.

-optional_callbacks([ observe_acl_is_allowed/2, pid_observe_acl_is_allowed/3 ]).

%% Check if a user is authorized to perform an action on a property.
%% Defaults to ``true``.
%% Type: first
%% Return: ``true`` to grant access, ``false`` to deny it, ``undefined`` to let the next observer decide
-callback observe_acl_is_allowed_prop(#acl_is_allowed_prop{}, z:context()) -> boolean() | undefined.
-callback pid_observe_acl_is_allowed_prop(pid(), #acl_is_allowed_prop{}, z:context()) -> boolean() | undefined.

-optional_callbacks([ observe_acl_is_allowed_prop/2, pid_observe_acl_is_allowed_prop/3 ]).

%% Set the context to a typical authenticated user. Used by m_acl.erl
%% Type: first
-callback observe_acl_context_authenticated(#acl_context_authenticated{}, z:context()) -> z:context() | undefined.
-callback pid_observe_acl_context_authenticated(pid(), #acl_context_authenticated{}, z:context()) -> z:context() | undefined.

-optional_callbacks([ observe_acl_context_authenticated/2, pid_observe_acl_context_authenticated/3 ]).

%% Initialize context with the access policy for the user.
%% Type: first
-callback observe_acl_logon(#acl_logon{}, z:context()) -> z:context() | undefined.
-callback pid_observe_acl_logon(pid(), #acl_logon{}, z:context()) -> z:context() | undefined.

-optional_callbacks([ observe_acl_logon/2, pid_observe_acl_logon/3 ]).

%% Clear the associated access policy for the context.
%% Type: first
-callback observe_acl_logoff(#acl_logoff{}, z:context()) -> z:context() | undefined.
-callback pid_observe_acl_logoff(pid(), #acl_logoff{}, z:context()) -> z:context() | undefined.

-optional_callbacks([ observe_acl_logoff/2, pid_observe_acl_logoff/3 ]).

%% Return the groups for the current user.
%% Type: first
-callback observe_acl_user_groups(#acl_user_groups{}, z:context()) -> Groups | undefined when
    Groups :: [ m_rsc:resource_id() ].
-callback pid_observe_acl_user_groups(pid(), #acl_user_groups{}, z:context()) -> Groups | undefined when
    Groups :: [ m_rsc:resource_id() ].

-optional_callbacks([ observe_acl_user_groups/2, pid_observe_acl_user_groups/3 ]).

%% Modify the list of user groups of a user. Called internally
%% by the ACL modules when fetching the list of user groups a user
%% is member of.
%% Type: foldl
-callback observe_acl_user_groups_modify(#acl_user_groups_modify{}, Acc, z:context()) -> Groups when
    Acc :: Groups,
    Groups :: [ m_rsc:resource_id() ].
-callback pid_observe_acl_user_groups_modify(pid(), #acl_user_groups_modify{}, Acc, z:context()) -> Groups when
    Acc :: Groups,
    Groups :: [ m_rsc:resource_id() ].

-optional_callbacks([ observe_acl_user_groups_modify/3, pid_observe_acl_user_groups_modify/4 ]).

%% Modify the list of collaboration groups of a user. Called internally
%% by the ACL modules when fetching the list of collaboration groups a user
%% is member of.
%% Type: foldl
-callback observe_acl_collab_groups_modify(#acl_collab_groups_modify{}, Acc, z:context()) -> Groups when
    Acc :: Groups,
    Groups :: [ m_rsc:resource_id() ].
-callback pid_observe_acl_collab_groups_modify(pid(), #acl_collab_groups_modify{}, Acc, z:context()) -> Groups when
    Acc :: Groups,
    Groups :: [ m_rsc:resource_id() ].

-optional_callbacks([ observe_acl_collab_groups_modify/3, pid_observe_acl_collab_groups_modify/4 ]).

%% Confirm a user id.
%% Type: foldl
-callback observe_auth_confirm(#auth_confirm{}, Acc, z:context()) -> Result when
    Acc :: z:context(),
    Result :: z:context().
-callback pid_observe_auth_confirm(pid(), #auth_confirm{}, Acc, z:context()) -> Result when
    Acc :: z:context(),
    Result :: z:context().

-optional_callbacks([ observe_auth_confirm/3, pid_observe_auth_confirm/4 ]).

% %% A user id has been confirmed.
% %% Type: notify
-callback observe_auth_confirm_done(#auth_confirm_done{}, z:context()) -> any().
-callback pid_observe_auth_confirm_done(pid(), #auth_confirm_done{}, z:context()) -> any().

-optional_callbacks([ observe_auth_confirm_done/2, pid_observe_auth_confirm_done/3 ]).

%% First for logon of user with username, check for ratelimit, blocks etc.
%% Type: first
-callback observe_auth_precheck(#auth_precheck{}, z:context()) -> Result when
    Result :: ok
            | {error, term()}
            | undefined.
-callback pid_observe_auth_precheck(pid(), #auth_precheck{}, z:context()) -> Result when
    Result :: ok
            | {error, term()}
            | undefined.

-optional_callbacks([ observe_auth_precheck/2, pid_observe_auth_precheck/3 ]).

%% First for logon of user with username, called after successful password check.
%% Type: first
-callback observe_auth_postcheck(#auth_postcheck{}, z:context()) -> Result when
    Result :: ok
            | {error, term()}
            | undefined.
-callback pid_observe_auth_postcheck(pid(), #auth_postcheck{}, z:context()) -> Result when
    Result :: ok
            | {error, term()}
            | undefined.

-optional_callbacks([ observe_auth_postcheck/2, pid_observe_auth_postcheck/3 ]).

%% Notify after logon of user with username, communicates valid or invalid password
%% Type: notify_sync
-callback observe_auth_checked(#auth_checked{}, z:context()) -> any().
-callback pid_observe_auth_checked(pid(), #auth_checked{}, z:context()) -> any().

-optional_callbacks([ observe_auth_checked/2, pid_observe_auth_checked/3 ]).

%% First to check for password reset forms, return undefined, ok, or {error, Reason}.
%% Type: first
-callback observe_auth_reset(#auth_reset{}, z:context()) -> Result when
    Result :: ok
            | {error, term()}
            | undefined.
-callback pid_observe_auth_reset(pid(), #auth_reset{}, z:context()) -> Result when
    Result :: ok
            | {error, term()}
            | undefined.

-optional_callbacks([ observe_auth_reset/2, pid_observe_auth_reset/3 ]).

%% First to validate a password. Return {ok, RscId} or {error, Reason}.
%% Type: first
-callback observe_auth_validate(#auth_validate{}, z:context()) -> Result when
    Result :: {ok, m_rsc:resource_id()}
            | {error, term()}
            | undefined.
-callback pid_observe_auth_validate(pid(), #auth_validate{}, z:context()) -> Result when
    Result :: {ok, m_rsc:resource_id()}
            | {error, term()}
            | undefined.

-optional_callbacks([ observe_auth_validate/2, pid_observe_auth_validate/3 ]).

%% User logs on. Add user-related properties to the logon request context.
%% Type: foldl
-callback observe_auth_logon(#auth_logon{}, Acc, z:context()) -> Result when
    Acc :: z:context(),
    Result :: z:context().
-callback pid_observe_auth_logon(pid(), #auth_logon{}, Acc, z:context()) -> Result when
    Acc :: z:context(),
    Result :: z:context().

-optional_callbacks([ observe_auth_logon/3, pid_observe_auth_logon/4 ]).

%% User is about to log off. Modify (if needed) the logoff request context.
%% Type: foldl
-callback observe_auth_logoff(#auth_logoff{}, Acc, z:context()) -> Result when
    Acc :: z:context(),
    Result :: z:context().
-callback pid_observe_auth_logoff(pid(), #auth_logoff{}, Acc, z:context()) -> Result when
    Acc :: z:context(),
    Result :: z:context().

-optional_callbacks([ observe_auth_logoff/3, pid_observe_auth_logoff/4 ]).

%% Authentication against some (external or internal) service was validated
%% Type: first
%% TODO: check when Context return is expected and when RscId
-callback observe_auth_validated(#auth_validated{}, z:context()) -> Result when
    Result :: {ok, m_rsc:resource_id()}
            | {ok, z:context()}
            | {error, term()}
            | undefined.
-callback pid_observe_auth_validated(pid(), #auth_validated{}, z:context()) -> Result when
    Result :: {ok, m_rsc:resource_id()}
            | {ok, z:context()}
            | {error, term()}
            | undefined.

-optional_callbacks([ observe_auth_validated/2, pid_observe_auth_validated/3 ]).

%% Update the given (accumulator) authentication options with the request options.
%%      Note that the request options are from the client and are unsafe.
%% Type: foldl
-callback observe_auth_options_update(#auth_options_update{}, Acc, z:context()) -> Result when
    Acc :: map(),
    Result :: map().
-callback pid_observe_auth_options_update(pid(), #auth_options_update{}, Acc, z:context()) -> Result when
    Acc :: map(),
    Result :: map().

-optional_callbacks([ observe_auth_options_update/3, pid_observe_auth_options_update/4 ]).

%% Send a request to the client to login a user. The zotonic.auth.worker.js will
%%      send a request to controller_authentication to exchange the one time token with
%%      a z.auth cookie for the given user. The client will redirect to the Url.
%% Type: first
%% Return: ``ok | {error, term()}``
-callback observe_auth_client_logon_user(#auth_client_logon_user{}, z:context()) -> Result when
    Result :: ok
            | {error, term()}
            | undefined.
-callback pid_observe_auth_client_logon_user(pid(), #auth_client_logon_user{}, z:context()) -> Result when
    Result :: ok
            | {error, term()}
            | undefined.

-optional_callbacks([ observe_auth_client_logon_user/2, pid_observe_auth_client_logon_user/3 ]).

%% Send a request to the client to switch users. The zotonic.auth.worker.js will
%%      send a request to controller_authentication to perform the switch.
%% Type: first
-callback observe_auth_client_switch_user(#auth_client_switch_user{}, z:context()) -> Result when
    Result :: ok
            | {error, term()}
            | undefined.
-callback pid_observe_auth_client_switch_user(pid(), #auth_client_switch_user{}, z:context()) -> Result when
    Result :: ok
            | {error, term()}
            | undefined.

-optional_callbacks([ observe_auth_client_switch_user/2, pid_observe_auth_client_switch_user/3 ]).


%% Return the list of identity types that allow somebody to logon and become an
%% active user of the system. Defaults to [ username_pw ].  In the future more types
%% can be requested, think of 'contact' - to be able to contact someone.
%% Type: foldl
-callback observe_auth_identity_types(#auth_identity_types{}, Acc, z:context()) -> Result when
    Acc :: [ atom() ],
    Result :: [ atom() ].
-callback pid_observe_auth_identity_types(pid(), #auth_identity_types{}, Acc, z:context()) -> Result when
    Acc :: [ atom() ],
    Result :: [ atom() ].

-optional_callbacks([ observe_auth_identity_types/3, pid_observe_auth_identity_types/4 ]).

%% Called during different moments of the request.
%%      * init - called on every http request
%%      * refresh - called after init and on mqtt context updates
%%      * auth_status - called on every authentication status poll
%% Type: foldl
-callback observe_request_context(#request_context{}, Acc, z:context()) -> Result when
    Acc :: z:context(),
    Result :: z:context().
-callback pid_observe_request_context(pid(), #request_context{}, Acc, z:context()) -> Result when
    Acc :: z:context(),
    Result :: z:context().

-optional_callbacks([ observe_request_context/3, pid_observe_request_context/4 ]).

%% Refresh the context or request process for the given request or action
%%      Called for every request that is not anonymous and before every MQTT relay from
%%      the client.  Example: mod_development uses this to set flags in the process
%%      dictionary.
%% Type: foldl
-callback observe_session_context(#session_context{}, Acc, z:context()) -> Result when
    Acc :: z:context(),
    Result :: z:context().
-callback pid_observe_session_context(pid(), #session_context{}, Acc, z:context()) -> Result when
    Acc :: z:context(),
    Result :: z:context().

-optional_callbacks([ observe_session_context/3, pid_observe_session_context/4 ]).


%% Called just before validation of all query arguments by z_validation.
%%      This is the moment to filter any illegal arguments or change query
%%      arguments.
%% Type: foldl
-callback observe_validate_query_args(#validate_query_args{}, Acc, z:context()) -> Result when
    Acc :: {ok, Args} | {error, term()},
    Result :: {ok, Args} | {error, term()},
    Args :: [ {binary(), z:qvalue()} ].
-callback pid_observe_validate_query_args(pid(), #validate_query_args{}, Acc, z:context()) -> Result when
    Acc :: {ok, Args} | {error, term()},
    Result :: {ok, Args} | {error, term()},
    Args :: [ {binary(), z:qvalue()} ].

-optional_callbacks([ observe_validate_query_args/3, pid_observe_validate_query_args/4 ]).

%% Check if a user is enabled. Enabled users are allowed to log in.
%% Type: first
%% Return ``true``, ``false`` or ``undefined``. If ``undefined`` is returned,
%% the user is considered enabled if the user resource is published.
-callback observe_user_is_enabled(#user_is_enabled{}, z:context()) -> boolean() | undefined.
-callback pid_observe_user_is_enabled(pid(), #user_is_enabled{}, z:context()) -> boolean() | undefined.

-optional_callbacks([ observe_user_is_enabled/2, pid_observe_user_is_enabled/3 ]).

%% Set #context fields depending on the user and/or the preferences of the user.
%% Type: foldl
-callback observe_user_context(#user_context{}, Acc, z:context()) -> Result when
    Acc :: z:context(),
    Result :: z:context().
-callback pid_observe_user_context(pid(), #user_context{}, Acc, z:context()) -> Result when
    Acc :: z:context(),
    Result :: z:context().

-optional_callbacks([ observe_user_context/3, pid_observe_user_context/4 ]).

%% Fetch the url of a resource's html representation
%% Type: first
%% Return: ``{ok, Url}`` or ``undefined``
-callback observe_page_url(#page_url{}, z:context()) -> {ok, binary()} | undefined.
-callback pid_observe_page_url(pid(), #page_url{}, z:context()) -> {ok, binary()} | undefined.

-optional_callbacks([ observe_page_url/2, pid_observe_page_url/3 ]).

%% Handle custom named search queries in your function.
%% Type: first
-callback observe_search_query(#search_query{}, z:context()) -> Result when
    Result :: #search_sql{}
            | #search_result{}
            | list()
            | undefined.
-callback pid_observe_search_query(pid(), #search_query{}, z:context()) -> Result when
    Result :: #search_sql{}
            | #search_result{}
            | list()
            | undefined.

-optional_callbacks([ observe_search_query/2, pid_observe_search_query/3 ]).

%% Map a custom search term to a ``#search_sql_term{}`` record.
%% Type: first
-callback observe_search_query_term(#search_query_term{}, z:context()) -> Result when
    Result :: #search_sql_term{}
            | QueryTerm
            | [ QueryTerm ]
            | undefined,
    QueryTerm :: #{ binary() => term() }.
-callback pid_observe_search_query_term(pid(), #search_query_term{}, z:context()) -> Result when
    Result :: #search_sql_term{}
            | QueryTerm
            | [ QueryTerm ]
            | undefined,
    QueryTerm :: #{ binary() => term() }.

-optional_callbacks([ observe_search_query_term/2, pid_observe_search_query_term/3 ]).

%% An edge has been inserted.
%% Note that the Context for this notification does not have the user who
%% created the edge.
%% Type: notify
-callback observe_edge_insert(#edge_insert{}, z:context()) -> any().
-callback pid_observe_edge_insert(pid(), #edge_insert{}, z:context()) -> any().

-optional_callbacks([ observe_edge_insert/2, pid_observe_edge_insert/3 ]).

%% An edge has been deleted
%% Note that the Context for this notification does not have the user who
%% deleted the edge.
%% Type: notify
-callback observe_edge_delete(#edge_delete{}, z:context()) -> any().
-callback pid_observe_edge_delete(pid(), #edge_delete{}, z:context()) -> any().

-optional_callbacks([ observe_edge_delete/2, pid_observe_edge_delete/3 ]).

%% An edge has been updated
%% Note that the Context for this notification does not have the user who
%% updated the edge.
%% Type: notify
-callback observe_edge_update(#edge_update{}, z:context()) -> any().
-callback pid_observe_edge_update(pid(), #edge_update{}, z:context()) -> any().

-optional_callbacks([ observe_edge_update/2, pid_observe_edge_update/3 ]).

%% Site configuration parameter was changed
%% Type: notify
-callback observe_m_config_update(#m_config_update{}, z:context()) -> any().
-callback pid_observe_m_config_update(pid(), #m_config_update{}, z:context()) -> any().

-optional_callbacks([ observe_m_config_update/2, pid_observe_m_config_update/3 ]).

% %% Site configuration parameter was changed
% %% Type: notify
-callback observe_m_config_update_prop(#m_config_update_prop{}, z:context()) -> any().
-callback pid_observe_m_config_update_prop(pid(), #m_config_update_prop{}, z:context()) -> any().

-optional_callbacks([ observe_m_config_update_prop/2, pid_observe_m_config_update_prop/3 ]).

%% Fetch the data for an import of a resource. Returns data in the format
%% used by m_rsc_export and m_rsc_import. Either returns the JSON data, the
%% imported resource id, or the resource id and a map with a mapping from URIs to
%% resource ids.
%% Type: first
-callback observe_rsc_import_fetch(#rsc_import_fetch{}, z:context()) -> Result when
    Result :: {ok, map()}
            | {ok, m_rsc:resource_id()}
            | {ok, {m_rsc:resource_id(), map()}}
            | {error, term()}
            | undefined.
-callback pid_observe_rsc_import_fetch(pid(), #rsc_import_fetch{}, z:context()) -> Result when
    Result :: {ok, map()}
            | {ok, m_rsc:resource_id()}
            | {ok, {m_rsc:resource_id(), map()}}
            | {error, term()}
            | undefined.

-optional_callbacks([ observe_rsc_import_fetch/2, pid_observe_rsc_import_fetch/3 ]).

%% Notification for fetching #media_import_props{} from different modules.
%% This is used by z_media_import.erl for fetching properties and medium information (map)
%% about resources.  The metadata is the result returned by z_url_metadata.
%% Type: map
-callback observe_media_import(#media_import{}, z:context()) -> Result when
    Result :: #media_import_props{}
            | [ #media_import_props{} ]
            | undefined.
-callback pid_observe_media_import(pid(), #media_import{}, z:context()) -> Result when
    Result :: #media_import_props{}
            | [ #media_import_props{} ]
            | undefined.

-optional_callbacks([ observe_media_import/2, pid_observe_media_import/3 ]).

%% Notification to translate or map a file after upload, before insertion into the database
%% Used in mod_video to queue movies for conversion to mp4.
%% You can set the post_insert_fun to something like fun(Id, Medium, Context) to receive the
%% medium record as it is inserted.
%% Type: first
-callback observe_media_upload_preprocess(#media_upload_preprocess{}, z:context()) -> Result when
    Result :: #media_upload_preprocess{}
            | undefined.
-callback pid_observe_media_upload_preprocess(pid(), #media_upload_preprocess{}, z:context()) -> Result when
    Result :: #media_upload_preprocess{}
            | undefined.

-optional_callbacks([ observe_media_upload_preprocess/2, pid_observe_media_upload_preprocess/3 ]).

%% Notification that a medium file has been uploaded.
%% This is the moment to change properties, modify the file etc.
%% The folded accumulator is the map with updated medium properties.
%% Type: foldl
-callback observe_media_upload_props(#media_upload_props{}, Acc, z:context()) -> Result when
    Acc :: MediumRecord,
    Result :: MediumRecord,
    MediumRecord :: #{ binary() => term() }.
-callback pid_observe_media_upload_props(pid(), #media_upload_props{}, Acc, z:context()) -> Result when
    Acc :: MediumRecord,
    Result :: MediumRecord,
    MediumRecord :: #{ binary() => term() }.

-optional_callbacks([ observe_media_upload_props/3, pid_observe_media_upload_props/4 ]).

%% Notification that a medium file has been uploaded.
%% This is the moment to change resource properties, modify the file etc.
%% The folded accumulator is the map with updated resource properties.
%% Type: foldl
-callback observe_media_upload_rsc_props(#media_upload_rsc_props{}, Acc, z:context()) -> Result when
    Acc :: RscProps,
    Result :: RscProps,
    RscProps :: m_rsc:props().
-callback pid_observe_media_upload_rsc_props(pid(), #media_upload_rsc_props{}, Acc, z:context()) -> Result when
    Acc :: RscProps,
    Result :: RscProps,
    RscProps :: m_rsc:props().

-optional_callbacks([ observe_media_upload_rsc_props/3, pid_observe_media_upload_rsc_props/4 ]).

%% Notification to import a medium record from external source. This is called for non-file
%% medium records, for example embedded video.  If the medium record is not recognized then it
%% will not be imported. The handling module is responsible for sanitizing and inserting the medium
%% record.
%% Type: first
-callback observe_media_import_medium(#media_import_medium{}, z:context()) -> Result when
    Result :: ok
            | {error, term()}
            | undefined.
-callback pid_observe_media_import_medium(pid(), #media_import_medium{}, z:context()) -> Result when
    Result :: ok
            | {error, term()}
            | undefined.

-optional_callbacks([ observe_media_import_medium/2, pid_observe_media_import_medium/3 ]).

%% Notification that a medium file has been changed (notify)
%% The id is the resource id, medium contains the medium's property list.
%% Type: notify
-callback observe_media_replace_file(#media_replace_file{}, z:context()) -> any().
-callback pid_observe_media_replace_file(pid(), #media_replace_file{}, z:context()) -> any().

-optional_callbacks([ observe_media_replace_file/2, pid_observe_media_replace_file/3 ]).

%% Media update done notification. action is 'insert', 'update' or 'delete'
%% Type: notify
-callback observe_media_update_done(#media_update_done{}, z:context()) -> any().
-callback pid_observe_media_update_done(pid(), #media_update_done{}, z:context()) -> any().

-optional_callbacks([ observe_media_update_done/2, pid_observe_media_update_done/3 ]).

%% Modify the options for an image preview url or tag. This is called for every
%% image url generation, except if the 'original' image option is passed. The mediaclass
%% in the options is not yet expanded.
%% Type: foldl
-callback observe_media_preview_options(#media_preview_options{}, Acc, z:context()) -> Result when
    Acc :: ImageOptions,
    Result :: ImageOptions,
    ImageOptions :: proplists:proplist().
-callback pid_observe_media_preview_options(pid(), #media_preview_options{}, Acc, z:context()) -> Result when
    Acc :: ImageOptions,
    Result :: ImageOptions,
    ImageOptions :: proplists:proplist().

-optional_callbacks([ observe_media_preview_options/3, pid_observe_media_preview_options/4 ]).

%% Request a translation of a list of strings. The resulting translations must
%% be in the same order as the request. This notification is handled by modules
%% that interface to external translation services like DeepL or Google Translate.
%% Type: first
-callback observe_translate(#translate{}, z:context()) -> Result when
    Result :: {ok, Translations}
            | {error, term()}
            | undefined,
    Translations :: [ {binary(), undefined | binary()} ].
-callback pid_observe_translate(pid(), #translate{}, z:context()) -> Result when
    Result :: {ok, Translations}
            | {error, term()}
            | undefined,
    Translations :: [ {binary(), undefined | binary()} ].

-optional_callbacks([ observe_translate/2, pid_observe_translate/3 ]).

%% Try to detect the language of a translation. Set is_editable_only to false
%% to detect any language, even if the language is not enabled for the site.
%% Type: first
-callback observe_language_detect(#language_detect{}, z:context()) -> Result when
    Result :: z_language:language_code()
            | undefined.
-callback pid_observe_language_detect(pid(), #language_detect{}, z:context()) -> Result when
    Result :: z_language:language_code()
            | undefined.

-optional_callbacks([ observe_language_detect/2, pid_observe_language_detect/3 ]).

% %% Send a notification that the resource 'id' is added to the query query_id.
% %% Type: notify
-callback observe_rsc_query_item(#rsc_query_item{}, z:context()) -> any().
-callback pid_observe_rsc_query_item(pid(), #rsc_query_item{}, z:context()) -> any().

-optional_callbacks([ observe_rsc_query_item/2, pid_observe_rsc_query_item/3 ]).

%% Add extra javascript with the {% script %} tag. (map)
%% Used to let modules inject extra javascript depending on the arguments of the {% script %} tag.
%% Type: map
-callback observe_scomp_script_render(#scomp_script_render{}, z:context()) -> iodata().
-callback pid_observe_scomp_script_render(pid(), #scomp_script_render{}, z:context()) -> iodata().

-optional_callbacks([ observe_scomp_script_render/2, pid_observe_scomp_script_render/3 ]).

%% Render the javascript for a custom action event type.
%% The custom event type must be a tuple, for example:
%% ``{% wire type={live id=myid} action={...} %}</code>``
%% Type: first
-callback observe_action_event_type(#action_event_type{}, z:context()) -> Result when
    Result :: {ok, Javascript, z:context()}
            | undefined,
    Javascript :: iodata().
-callback pid_observe_action_event_type(pid(), #action_event_type{}, z:context()) -> Result when
    Result :: {ok, Javascript, z:context()}
            | undefined,
    Javascript :: iodata().

-optional_callbacks([ observe_action_event_type/2, pid_observe_action_event_type/3 ]).

%% Find an import definition for a CSV file by checking the filename of the to be imported file.
%% Type: first
-callback observe_import_csv_definition(#import_csv_definition{}, z:context()) -> Result when
    Result :: {ok, #import_data_def{}}
            | ok
            | {error, term()}
            | undefined.
-callback pid_observe_import_csv_definition(pid(), #import_csv_definition{}, z:context()) -> Result when
    Result :: {ok, #import_data_def{}}
            | ok
            | {error, term()}
            | undefined.

-optional_callbacks([ observe_import_csv_definition/2, pid_observe_import_csv_definition/3 ]).

%% Handle a new file received in the 'files/dropbox' folder of a site.
%% Unhandled files are deleted after an hour. If the handler returns 'ok' then
%% the file is moved from the files/processing folder to files/handled.
%% Type: first
-callback observe_dropbox_file(#dropbox_file{}, z:context()) -> Result when
    Result :: ok
            | {ok, processing}
            | undefined.
-callback pid_observe_dropbox_file(pid(), #dropbox_file{}, z:context()) -> Result when
    Result :: ok
            | {ok, processing}
            | undefined.

-optional_callbacks([ observe_dropbox_file/2, pid_observe_dropbox_file/3 ]).

%% Try to identify a file, returning a map with file properties.
%% Most interesting keys for the returned map:
%% ``<<"mime">>``, ``<<"width">>``, ``<<"height">>``, ``<<"orientation">>``
%% Type: first
-callback observe_media_identify_file(#media_identify_file{}, z:context()) -> Result when
    Result :: MimeData
            | undefined,
    MimeData :: #{ binary() => term() }.
-callback pid_observe_media_identify_file(pid(), #media_identify_file{}, z:context()) -> Result when
    Result :: MimeData
            | undefined,
    MimeData :: #{ binary() => term() }.

-optional_callbacks([ observe_media_identify_file/2, pid_observe_media_identify_file/3 ]).

%% Try to find a filename extension for a mime type (example: ``<<".jpg">>``)
%% Type: first
-callback observe_media_identify_extension(#media_identify_extension{}, z:context()) -> Result when
    Result :: Extension
            | undefined,
    Extension ::binary().
-callback pid_observe_media_identify_extension(pid(), #media_identify_extension{}, z:context()) -> Result when
    Result :: Extension
            | undefined,
    Extension ::binary().

-optional_callbacks([ observe_media_identify_extension/2, pid_observe_media_identify_extension/3 ]).

%% Request to generate a HTML media viewer for a resource. The HTML data can not contain any
%% Javascript, as it might be serialized. This could happen if the correct cookies are not yet
%% set or if the media viewer is part of a direct DOM update.
%% Type: first
-callback observe_media_viewer(#media_viewer{}, z:context()) -> Result when
    Result :: {ok, HTML}
            | undefined,
    HTML :: iodata().
-callback pid_observe_media_viewer(pid(), #media_viewer{}, z:context()) -> Result when
    Result :: {ok, HTML}
            | undefined,
    HTML :: iodata().

-optional_callbacks([ observe_media_viewer/2, pid_observe_media_viewer/3 ]).

%% See if there is a 'still' image preview of a media item. (eg posterframe of a movie)
%% Type: first
-callback observe_media_stillimage(#media_stillimage{}, z:context()) -> Result when
    Result :: {ok, m_rsc:resource_id()}
            | undefined.
-callback pid_observe_media_stillimage(pid(), #media_stillimage{}, z:context()) -> Result when
    Result :: {ok, m_rsc:resource_id()}
            | undefined.

-optional_callbacks([ observe_media_stillimage/2, pid_observe_media_stillimage/3 ]).

%% Optionally wrap HTML with external content so that it adheres to the cookie/privacy
%% settings of the current site visitor. Typically called with a 'first' by the code that
%% generated the media viewer HTML, as that code has the knowledge if viewing the generated code
%% has any privacy or cookie implications.
%% Type: first
-callback observe_media_viewer_consent(#media_viewer_consent{}, z:context()) -> Result when
    Result :: {ok, HTML}
            | undefined,
    HTML :: iodata().
-callback pid_observe_media_viewer_consent(pid(), #media_viewer_consent{}, z:context()) -> Result when
    Result :: {ok, HTML}
            | undefined,
    HTML :: iodata().

-optional_callbacks([ observe_media_viewer_consent/2, pid_observe_media_viewer_consent/3 ]).

%% Fetch list of handlers for survey submits.
%% Type: foldr
-callback observe_survey_get_handlers(#survey_get_handlers{}, Acc, z:context()) -> Result when
    Acc :: Handlers,
    Result :: Handlers,
    Handlers :: [ {HandlerName, DisplayTitle} ],
    HandlerName :: atom(),
    DisplayTitle :: binary().
-callback pid_observe_survey_get_handlers(pid(), #survey_get_handlers{}, Acc, z:context()) -> Result when
    Acc :: Handlers,
    Result :: Handlers,
    Handlers :: [ {HandlerName, DisplayTitle} ],
    HandlerName :: atom(),
    DisplayTitle :: binary().

-optional_callbacks([ observe_survey_get_handlers/3, pid_observe_survey_get_handlers/4 ]).

%% A survey has been filled in and submitted.
%% Type: first
-callback observe_survey_submit(#survey_submit{}, z:context()) -> Result when
    Result :: ok
            | {ok, z:context() | #render{}}
            | {save, z:context() | #render{}}
            | {error, term()}
            | undefined.
-callback pid_observe_survey_submit(pid(), #survey_submit{}, z:context()) -> Result when
    Result :: ok
            | {ok, z:context() | #render{}}
            | {save, z:context() | #render{}}
            | {error, term()}
            | undefined.

-optional_callbacks([ observe_survey_submit/2, pid_observe_survey_submit/3 ]).

%% Check if the current user is allowed to download a survey.
%% Type: first
-callback observe_survey_is_allowed_results_download(#survey_is_allowed_results_download{}, z:context()) -> Result when
    Result ::boolean()
            | undefined.
-callback pid_observe_survey_is_allowed_results_download(pid(), #survey_is_allowed_results_download{}, z:context()) -> Result when
    Result :: boolean()
            | undefined.

-optional_callbacks([ observe_survey_is_allowed_results_download/2, pid_observe_survey_is_allowed_results_download/3 ]).

%% Check if a question (page block) is a submitting question.
%% Type: first
-callback observe_survey_is_submit(#survey_is_submit{}, z:context()) -> Result when
    Result ::boolean()
            | undefined.
-callback pid_observe_survey_is_submit(pid(), #survey_is_submit{}, z:context()) -> Result when
    Result :: boolean()
            | undefined.

-optional_callbacks([ observe_survey_is_submit/2, pid_observe_survey_is_submit/3 ]).

%% Add header columns for export. The values are the names of the answers and
%% the text displayed above the column. The ``text`` format is for a complete export, the
%% ``html`` format is for the limited result overview of the Survey Results Editor.
%% Type: foldl
-callback observe_survey_result_columns(#survey_result_columns{}, Acc, z:context()) -> Result when
    Acc :: Columns,
    Result :: Columns,
    Columns :: [ {QuestionName, Title} ],
    QuestionName :: binary(),
    Title :: binary() | z:trans().
-callback pid_observe_survey_result_columns(pid(), #survey_result_columns{}, Acc, z:context()) -> Result when
    Acc :: Columns,
    Result :: Columns,
    Columns :: [ {QuestionName, Title} ],
    QuestionName :: binary(),
    Title :: binary() | z:trans().

-optional_callbacks([ observe_survey_result_columns/3, pid_observe_survey_result_columns/4 ]).

%% Modify row with answers for export. The header columns are given and the
%% values that are known are set in the folded value. The user_id is the user who
%% filled in the answers for this row.
%% Type: foldl
-callback observe_survey_result_column_values(#survey_result_column_values{}, Acc, z:context()) -> Result when
    Acc :: ColumnValues,
    Result :: ColumnValues,
    ColumnValues :: #{ QuestionName => Value },
    QuestionName :: binary(),
    Value :: iodata().
-callback pid_observe_survey_result_column_values(pid(), #survey_result_column_values{}, Acc, z:context()) -> Result when
    Acc :: ColumnValues,
    Result :: ColumnValues,
    ColumnValues :: #{ QuestionName => Value },
    QuestionName :: binary(),
    Value :: iodata().

-optional_callbacks([ observe_survey_result_column_values/3, pid_observe_survey_result_column_values/4 ]).

%% Put a value into the typed key/value store
%% Type: first
-callback observe_tkvstore_put(#tkvstore_put{}, z:context()) -> ok | undefined.
-callback pid_observe_tkvstore_put(pid(), #tkvstore_put{}, z:context()) -> ok | undefined.

-optional_callbacks([ observe_tkvstore_put/2, pid_observe_tkvstore_put/3 ]).

%% Get a value from the typed key/value store
%% Type: first
-callback observe_tkvstore_get(#tkvstore_get{}, z:context()) -> term() | undefined.
-callback pid_observe_tkvstore_get(pid(), #tkvstore_get{}, z:context()) -> term() | undefined.

-optional_callbacks([ observe_tkvstore_get/2, pid_observe_tkvstore_get/3 ]).

%% Delete a value from the typed key/value store
%% Type: notify
-callback observe_tkvstore_delete(#tkvstore_delete{}, z:context()) -> any().
-callback pid_observe_tkvstore_delete(pid(), #tkvstore_delete{}, z:context()) -> any().

-optional_callbacks([ observe_tkvstore_delete/2, pid_observe_tkvstore_delete/3 ]).

%% Push some information to the debug page in the user-agent.
%% Will be displayed with io_lib:format("~p: ~p~n", [What, Arg]), be careful with escaping information!
-callback observe_debug(#debug{}, z:context()) -> any().
-callback pid_observe_debug(pid(), #debug{}, z:context()) -> any().

-optional_callbacks([ observe_debug/2, pid_observe_debug/3 ]).

%% Broadcast some file changed, used for livereload by mod_development
%% Type: notify
-callback observe_filewatcher(#filewatcher{}, z:context()) -> any().
-callback pid_observe_filewatcher(pid(), #filewatcher{}, z:context()) -> any().

-optional_callbacks([ observe_filewatcher/2, pid_observe_filewatcher/3 ]).

%% An external feed delivered a resource. First handler can import it.
%% Type: first
-callback observe_import_resource(#import_resource{}, z:context()) -> Result when
    Result :: {ok, m_rsc:resource_id()}
            | {error, term()}
            | undefined.
-callback pid_observe_import_resource(pid(), #import_resource{}, z:context()) -> Result when
    Result :: {ok, m_rsc:resource_id()}
            | {error, term()}
            | undefined.

-optional_callbacks([ observe_import_resource/2, pid_observe_import_resource/3 ]).

%% mod_export - return the {ok, Disposition} for the content disposition.
%% The content disposition is either ``<<"inline">>`` or ``<<"attachment">>``.
%% Type: first
-callback observe_export_resource_content_disposition(#export_resource_content_disposition{}, z:context()) -> Result when
    Result :: {ok, Disposition}
            | undefined,
    Disposition :: binary().
-callback pid_observe_export_resource_content_disposition(pid(), #export_resource_content_disposition{}, z:context()) -> Result when
    Result :: {ok, Disposition}
            | undefined,
    Disposition :: binary().

-optional_callbacks([ observe_export_resource_content_disposition/2, pid_observe_export_resource_content_disposition/3 ]).

%% mod_export - Check if the resource or dispatch is visible for export.
%% Type: first
-callback observe_export_resource_visible(#export_resource_visible{}, z:context()) -> boolean() | undefined.
-callback pid_observe_export_resource_visible(pid(), #export_resource_visible{}, z:context()) -> boolean() | undefined.

-optional_callbacks([ observe_export_resource_visible/2, pid_observe_export_resource_visible/3 ]).

%% mod_export - Determine the mime type for the export.
%% Type: first
-callback observe_export_resource_content_type(#export_resource_content_type{}, z:context()) -> Result when
    Result :: {ok, binary() | string()}
            | undefined.
-callback pid_observe_export_resource_content_type(pid(), #export_resource_content_type{}, z:context()) -> Result when
    Result :: {ok, binary() | string()}
            | undefined.

-optional_callbacks([ observe_export_resource_content_type/2, pid_observe_export_resource_content_type/3 ]).

%% mod_export - return the {ok, Filename} for the content disposition.
%% Type: first
-callback observe_export_resource_filename(#export_resource_filename{}, z:context()) -> Result when
    Result :: {ok, binary() | string()}
            | undefined.
-callback pid_observe_export_resource_filename(pid(), #export_resource_filename{}, z:context()) -> Result when
    Result :: {ok, binary() | string()}
            | undefined.

-optional_callbacks([ observe_export_resource_filename/2, pid_observe_export_resource_filename/3 ]).

%% mod_export - Fetch the header for the export.
%% Type: first
-callback observe_export_resource_header(#export_resource_header{}, z:context()) -> Result when
    Result :: {ok, binary() | [ binary() ]}
            | {ok, binary() | [ binary() ], State}
            | {error, term()}
            | undefined,
    State :: term().
-callback pid_observe_export_resource_header(pid(), #export_resource_header{}, z:context()) -> Result when
    Result :: {ok, binary() | [ binary() ]}
            | {ok, binary() | [ binary() ], State}
            | {error, term()}
            | undefined,
    State :: term().

-optional_callbacks([ observe_export_resource_header/2, pid_observe_export_resource_header/3 ]).

%% mod_export - fetch a row for the export, can return a list of rows, a binary, and optionally a continuation state.
%% Where Values is [ term() ], i.e. a list of opaque values, to be formatted with #export_resource_format.
%% Return the empty list of values to signify the end of the data stream.
%% Type: first
-callback observe_export_resource_data(#export_resource_data{}, z:context()) -> Result when
    Result :: {ok, binary() | Values}
            | {ok, binary() | Values, State}
            | {error, term()}
            | undefined,
    Values :: [ term() ],
    State :: term().
-callback pid_observe_export_resource_data(pid(), #export_resource_data{}, z:context()) -> Result when
    Result :: {ok, binary() | Values}
            | {ok, binary() | Values, State}
            | {error, term()}
            | undefined,
    Values :: [ term() ],
    State :: term().

-optional_callbacks([ observe_export_resource_data/2, pid_observe_export_resource_data/3 ]).

%% mod_export - Encode a single data element.
%% Type: first
-callback observe_export_resource_encode(#export_resource_encode{}, z:context()) -> Result when
    Result :: {ok, binary()}
            | {ok, binary(), State}
            | {error, term()}
            | undefined,
    State :: term().
-callback pid_observe_export_resource_encode(pid(), #export_resource_encode{}, z:context()) -> Result when
    Result :: {ok, binary()}
            | {ok, binary(), State}
            | {error, term()}
            | undefined,
    State :: term().

-optional_callbacks([ observe_export_resource_encode/2, pid_observe_export_resource_encode/3 ]).

%% mod_export - Fetch the footer for the export. Should cleanup the continuation state, if needed.
%% Type: first
-callback observe_export_resource_footer(#export_resource_footer{}, z:context()) -> Result when
    Result :: {ok, binary()}
            | {error, term()}
            | undefined.
-callback pid_observe_export_resource_footer(pid(), #export_resource_footer{}, z:context()) -> Result when
    Result :: {ok, binary()}
            | {error, term()}
            | undefined.

-optional_callbacks([ observe_export_resource_footer/2, pid_observe_export_resource_footer/3 ]).

%% Handle a javascript notification from the postback handler. The ``message`` is the the request,
%% ``trigger`` the id of the element which triggered the postback, and ``target`` the
%% id of the element which should receive possible updates. ``#postback_notify`` is also used as an event.
%% Type: first
-callback observe_postback_notify(#postback_notify{}, z:context()) -> Result when
    Result :: z:context()
            | undefined.
-callback pid_observe_postback_notify(pid(), #postback_notify{}, z:context()) -> Result when
    Result :: z:context()
            | undefined.

-optional_callbacks([ observe_postback_notify/2, pid_observe_postback_notify/3 ]).

%% Determine the URL fetch options for fetching the content of an URL. Used by z_fetch.erl.
%% Type: first
-callback observe_url_fetch_options(#url_fetch_options{}, z:context()) -> Result when
    Result :: z_url_fetch:options()
            | undefined.
-callback pid_observe_url_fetch_options(pid(), #url_fetch_options{}, z:context()) -> Result when
    Result :: z_url_fetch:options()
            | undefined.

-optional_callbacks([ observe_url_fetch_options/2, pid_observe_url_fetch_options/3 ]).

%% Delegates the request processing.
%% Type: foldl
-callback observe_middleware(#middleware{}, Acc, z:context()) -> Result when
    Acc :: z:context(),
    Result :: z:context().
-callback pid_observe_middleware(pid(), #middleware{}, Acc, z:context()) -> Result when
    Acc :: z:context(),
    Result :: z:context().

-optional_callbacks([ observe_middleware/3, pid_observe_middleware/4 ]).

% Reload all template, modules etc. Triggered by manual request.
% Type: notify
-callback observe_development_reload(development_reload, z:context()) -> any().
-callback pid_observe_development_reload(pid(), development_reload, z:context()) -> any().

-optional_callbacks([ observe_development_reload/2, pid_observe_development_reload/3 ]).

% Perform a 'make' on Zotonic, reload all new beam files. Triggered by manual request.
% Type: notify
-callback observe_development_make(development_make, z:context()) -> any().
-callback pid_observe_development_make(pid(), development_make, z:context()) -> any().

-optional_callbacks([ observe_development_make/2, pid_observe_development_make/3 ]).
