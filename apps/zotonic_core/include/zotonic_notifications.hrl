%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2024 Marc Worrell
%% @doc Notifications used in Zotonic core
%% @end

%% Copyright 2011-2024 Marc Worrell
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

%% @doc Try to find the site for the request
%% Called when the request Host doesn't match any active site.
%% Type: first
%% Return: ``{ok, #dispatch_redirect{}}`` or ``undefined``
-record(dispatch_host, {
    host = <<>> :: binary(),
    path = <<>> :: binary(),
    method = <<"GET">> :: binary(),
    protocol = http :: http|https
}).

%% @doc Final try for dispatch, try to match the request.
%% Called when the site is known, but no match is found for the path
%% Type: first
%% Return: ``{ok, RscId::integer()}``, ``{ok, #dispatch_match{}}``, ``{ok, #dispatch_redirect{}}`` or ``undefined``
-record(dispatch, {
    host = <<>> :: binary(),
    path = <<>> :: binary(),
    method = <<"GET">> :: binary(),
    protocol = http :: http|https,
    tracer_pid = undefined :: pid() | undefined
}).


%% @doc Check and possibly modify the http response security headers
%% All headers are in lowercase.
%% Type: first
-record(security_headers, {
    headers :: list( {binary(), binary()} )
}).

%% @doc Set CORS headers on the HTTP response.
%% Type: first
-record(cors_headers, {
    headers :: list( {binary(), binary()} )
}).

%% @doc Let all modules add resource specific response headers to the request.
%% The accumulator is the list of headers to be set.
%% Type: foldl
%% Return: ``list( {binary(), binary()} )``
-record(resource_headers, {
    id :: m_rsc:resource_id() | undefined
}).

%% @doc Access log event for http. Called from the z_stats.
%% Type: notify_sync
-record(http_log_access, {
    timestamp :: erlang:timestamp(),
    status :: undefined | non_neg_integer(),
    status_category :: 'xxx' | '1xx' | '2xx' | '3xx' | '4xx' | '5xx',
    method :: binary(),
    metrics :: map()
}).

% 'module_ready' - Sent when modules have changed, z_module_indexer reindexes all modules' templates, actions etc.

%% @doc A module has been activated and started.
%% Type: notify
-record(module_activate, {
    module :: atom(),
    pid :: pid()
}).

%% @doc A module has been stopped and deactivated.
%% Type: notify
-record(module_deactivate, {
    module :: atom()
}).


%% @doc Get available content types and their dispatch rules
%% Example: {"text/html", page}
%% A special dispatch rule is 'page_url', which refers to the page_url property of the resource.
%% Type: foldr
%% Return: ``[{ContentType, DispatchRule}]``
-record(content_types_dispatch, {
    id :: m_rsc:resource()
}).

%% @doc Check where to go after a user logs on.
%% Type: first
%% Return: a URL or ``undefined``
-record(logon_ready_page, {
    request_page = <<>> :: binary() | undefined
}).


%% @doc Handle a user logon. The posted query args are included.
%% Type: first
%% Return:: ``{ok, UserId}`` or ``{error, Reason}``
-record(logon_submit, {
    payload = #{} :: #{ binary() => term() }
}).

%% @doc Check for logon options, called if logon_submit returns `undefined`.
%% This is used to fetch external (or local) authentication links for an
%% username.
%% Type: foldl
%% Return:: ``map()``
-record(logon_options, {
    payload = #{} :: #{ binary() => term() }
}).


%% @doc Request to send a verification to the user. Return ok or an error.
%% Handled by mod_signup to send out verification emails.
%% Type: first
%% Identity may be undefined, or is an identity used for the verification.
-record(identity_verification, {
    user_id :: m_rsc:resource_id(),
    identity :: undefined | m_identity:identity()
}).

%% @doc Notify that a user's identity has been verified. Signals to modules
%% handling identities to mark this identity as verified. Handled by mod_admin_identity
%% to call the m_identity model for this type/key.
%% Type: notify
-record(identity_verified, {
    user_id :: m_rsc:resource_id(),
    type :: m_identity:type(),
    key :: m_identity:key()
}).

%% @doc Check if passwords are matching. Uses the password hashing algorithms.
%% Type: first
-record(identity_password_match, {
    rsc_id :: m_rsc:resource_id() | undefined,
    password :: binary(),
    hash :: m_identity:hash() | {hash, atom() | binary(), binary()}
}).

%% @doc Notify that a user's identity has been updated by the identity model.
%% Type: notify
-record(identity_update_done, {
    action :: insert | update | delete | verify,
    rsc_id :: m_rsc:resource_id(),
    type :: binary(),
    key :: m_identity:key() | undefined,
    is_verified :: boolean() | undefined
}).


%% @doc Handle a signup of a user, return the follow on page for after the signup.
%% Type: first
%% Return ``{ok, Url}``
%% 'props' is a map with properties for the person resource (email, name, etc)
%% 'signup_props' is a proplist with 'identity' definitions and optional follow on url 'ready_page'
%% An identity definition is {Kind, Identifier, IsUnique, IsVerified}
-record(signup_url, {
    props = #{} :: map(),
    signup_props = [] :: list()
}).

%% @doc Request a signup of a new or existing user. Arguments are similar to #signup_url{}
%% Returns {ok, UserId} or {error, Reason}
-record(signup, {
    id :: m_rsc:resource_id() | undefined,
    props = #{} :: map(),
    signup_props = [] :: list(),
    request_confirm = false :: boolean()
}).

%% @doc Signup failed, give the error page URL. Return {ok, Url} or undefined.
%% Reason is returned by the signup handler for the particular signup method (username, facebook etc)
%% Type: first
-record(signup_failed_url, {
    reason
}).

%% signup_check
%% Check if the signup can be handled, a fold over all modules.
%% Fold argument/result is {ok, Props, SignupProps} or {error, Reason}
%% Type: foldl
%% Return: ``{ok, Props, SignupProps}`` or ``{error, Reason}``
-record(signup_check, {
    props = #{} :: map(),
    signup_props = [] :: list()
}).

%% @doc Signal that a user has been signed up (map, result is ignored)
%% Type: map
-record(signup_done, {
    id :: m_rsc:resource(),
    is_verified :: boolean(),
    props :: map(),
    signup_props :: list()
}).

%% @doc Signal that a user has been confirmed. (map, result is ignored)
%% Type: notify
-record(signup_confirm, {
    id :: m_rsc:resource()
}).

%% @doc Fetch the page a user is redirected to after signing up with a confirmed identity
%% Type: first
%% Return: a URL or ``undefined``
-record(signup_confirm_redirect, {
    id :: m_rsc:resource()
}).

%% @doc Notification to signal an inserted comment.
%% 'comment_id' is the id of the inserted comment, 'id' is the id of the resource commented on.
%% Type: notify
-record(comment_insert, {
    comment_id :: integer(),
    id :: m_rsc:resource_id()
}).

%% @doc Notify that the session's language has been changed
%% Type: notify
-record(language, {
    language :: atom()
}).

%% @doc Set the language of the context to a user's prefered language
%% Type: first
-record(set_user_language, {
    id :: m_rsc:resource_id()
}).

%% @doc Make a generated URL absolute, optionally called after url_rewrite by z_dispatcher
%% Type: first
-record(url_abs, {url, dispatch, dispatch_options}).

%% @doc Rewrite a URL after it has been generated using the z_dispatcher
%% Type: foldl
-record(url_rewrite, {
    dispatch :: atom(),
    args = [] :: list()
}).

%% @doc Rewrite a URL before it will be dispatched using the z_sites_dispatcher
%% Type: foldl
-record(dispatch_rewrite, {
    is_dir = false :: boolean(),
    path = <<>> :: binary(),
    host
}).

%% @doc Request the SSL certificates for this site. The server_name property contains the hostname used by the client. (first)
%% Returns either 'undefined' or a list of ssl options (type ssl:ssl_option())
-record(ssl_options, {server_name :: binary()}).

%% @doc Used in the admin to fetch the possible blocks for display
%% Type: foldl
-record(admin_edit_blocks, {
    id :: m_rsc:resource_id()
}).

%% @doc Used in the admin to process a submitted resource form
-record(admin_rscform, {
    id :: m_rsc:resource_id(),
    is_a :: list( atom() )
}).

%% @doc Used for fetching the menu in the admin.
%% Type: foldl
%% Return: list of admin menu items
-record(admin_menu, {}).

%% @doc Fetch the menu id belonging to a certain resource
%% Type: first
-record(menu_rsc, {
    id :: m_rsc:resource()
}).

%% @doc Fold for mapping non-iolist output to iolist values.
%%      Used when outputting a rendered HTML tree.
%%      Folded accumulator is: { MixedHtml, Context }
%% Type: foldl
-record(output_html, {
    html :: term()
}).


%% @doc An activity in Zotonic. When this is handled as a notification then return a list
%% of patterns matching this activity.  These patterns are then used to find interested
%% subscribers.
%% Type: map
-record(activity, {
    version = 1 :: pos_integer(),
    posted_time,
    actor,
    verb = post :: atom(),
    object, target
}).

%% @doc Push a list of activities via a 'channel' (eg 'email') to a recipient.
%% The activities are a list of #activity{} records.
%% Type: first
-record(activity_send, {
    recipient_id,
    channel,
    queue,
    activities = [] :: list()
}).


%% @doc Notification sent to a site when e-mail for that site is received
%% Type: first
-record(email_received, {
    to,
    from,
    localpart,
    localtags,
    domain,
    reference,
    email,
    headers,
    is_bulk = false :: boolean(),
    is_auto = false :: boolean(),
    decoded,
    raw
}).

% E-mail received notification:
% {z_convert:to_atom(Notification), received, UserId, ResourceId, Received}
% The {Notification, UserId, ResourceId} comes from m_email_receive_recipient:get_by_recipient/2.

%% @doc Check if an email address is blocked
%% Type: first
-record(email_is_blocked, {
    recipient :: binary()
}).

%% @doc Check if an email address is safe to send email to. The email address is not blocked
%% and is not marked as bouncing.
%% Type: first
-record(email_is_recipient_ok, {
    recipient :: binary()
}).

%% @doc Email status notification, sent when the validity of an email recipient changes
%% Type: notify
-record(email_status, {
    recipient :: binary(),
    is_valid :: boolean(),
    is_final :: boolean(),
    is_manual :: boolean()
}).

%% @doc Bounced e-mail notification.  The recipient is the e-mail that is bouncing. When the
%% the message_nr is unknown the it is set to 'undefined'. This can happen if it is a "late bounce".
%% If the recipient is defined then the Context is the depickled z_email:send/2 context.
%% Type: notify
-record(email_bounced, {
    message_nr :: binary() | undefined,
    recipient :: binary() | undefined
}).

%% @doc Notify that we could send an e-mail (there might be a bounce later...)
%% The Context is the depickled z_email:send/2 context.
%% Type: notify
-record(email_sent, {
    message_nr :: binary(),
    recipient :: binary(),
    is_final :: boolean()   % Set to true after waiting 4 hours for bounces
}).

%% @doc Notify that we could NOT send an e-mail (there might be a bounce later...)
%% The Context is the depickled z_email:send/2 context.
%% Type: notify
-record(email_failed, {
    message_nr :: binary(),
    recipient :: binary(),
    is_final :: boolean(),
    reason :: bounce | retry | illegal_address | smtphost | sender_disabled | error,
    retry_ct :: non_neg_integer() | undefined,
    status :: binary() | undefined
}).


%% @doc Return the options for the DKIM signature on outgoing emails. Called during
%% email encoding.
%% Type: first
%% Return: ``list()`` options for the DKIM signature
-record(email_dkim_options, {
}).

%% @doc Request to send an email using special email senders, for example using
%% proxy APIs. If no sender is found then the email is sent using the built-in smtp
%% server. The email is completely mime encoded.
%% The Context is the depickled z_email:send/2 context.
%% Type: first
%% Return: ``{ok, Status}`` where status is a binary; or
%%         ``smtp`` use the built-in smtp server; or
%%         ``{error, Reason::atom(), {FailureType, Host, Message}}`` when FailureType
%%         is one of ``permanent_failure`` or ``temporary_failure``.
-record(email_send_encoded, {
    message_nr :: binary(),
    from :: binary(),        % The envelop from
    to :: binary(),          % The envelop to
    encoded :: binary(),
    options :: gen_smtp_client:options()
}).


%% @doc Add a handler for receiving e-mail notifications
%% Type: first
%% Return: ``{ok, LocalFrom}``, the unique localpart of an e-mail address on this server.
-record(email_add_handler, {notification, user_id, resource_id}).
-record(email_ensure_handler, {notification, user_id, resource_id}).

%% @doc Drop an e-mail handler for a user/resource id. (notify).
%% The notification, user and resource should be the same as when the handler was registered.
-record(email_drop_handler, {notification, user_id, resource_id}).


%% @doc Send a page to a mailinglist (notify)
%% Use {single_test_address, Email} when sending to a specific e-mail address.
-record(mailinglist_mailing, {
    list_id = undefined :: m_rsc:resource() | undefined,
    email = undefined :: binary() | string() | undefined,
    page_id :: m_rsc:resource(),
    options = [] :: [ {is_match_language, boolean()} | {is_send_all, boolean()} ]
}).

%% @doc Send a welcome or goodbye message to the given recipient.
%% The recipient is either a recipient-id or a recipient props.
%% 'what' is send_welcome, send_confirm, send_goobye or silent.
%% Type: notify
-record(mailinglist_message, {
    what :: send_welcome | send_confirm | send_goodbye | silent,
    list_id :: m_rsc:resource(),
    recipient :: proplists:proplist() | integer()
}).

%% @doc Save (and update) the complete category hierarchy
%% Type: notify
-record(category_hierarchy_save, {tree}).

%% @doc Save the menu tree of a menu resource
%% Type: notify
-record(menu_save, {id, tree}).

%% @doc Signal that the hierarchy underneath a resource has been changed by mod_menu
%% Type: notify
-record(hierarchy_updated, {
    root_id :: binary() | integer(),
    predicate :: atom(),
    inserted_ids = [] :: list(integer()),
    deleted_ids = [] :: list(integer())
}).

%% @doc Resource is read, opportunity to add computed fields
%% Used in a foldr with the read properties as accumulator.
%% Type: foldr
-record(rsc_get, {
    id :: m_rsc:resource_id()
}).

%% @doc Resource will be deleted.
%% This notification is part of the delete transaction, it's purpose is to clean up
%% associated data.
%% Type: notify
-record(rsc_delete, {
    id :: m_rsc:resource_id(),
    is_a :: list( atom() )
}).

%% @doc Foldr for an resource insert, these are the initial properties and will overrule
%% the properties in the insert request. Use with care.  The props are the properties of
%% the later insert, after escaping/filtering but before the #rsc_update{} notification below.
%% Type: foldr
%% Return: proplist accumulator
-record(rsc_insert, {
    props :: m_rsc:props()
}).

%% @doc Map to signal merging two resources. Move any information from the loser to the
%% winner. The loser will be deleted.
%% Type: map
-record(rsc_merge, {
    winner_id :: m_rsc:resource_id(),
    loser_id :: m_rsc:resource_id(),
    is_merge_trans :: boolean()
}).

%% @doc An updated resource is about to be persisted.
%% Observe this notification to change the resource properties before they are
%% persisted.
%%
%% The props are the resource's props _before_ the update, but _after_ filtering
%% and sanitization. The folded value is ``{ok, UpdateProps}`` for the update itself.
%% Type: foldr
%% Return: ``{ok, UpdateProps}`` or ``{error, term()}``
-record(rsc_update, {
    action :: insert | update,
    id :: m_rsc:resource_id(),
    props :: m_rsc:props()
}).

%% @doc An updated resource has just been persisted. Observe this notification to
%% execute follow-up actions for a resource update.
%% Type: notify
%% Return: return value is ignored
-record(rsc_update_done, {
    action :: insert | update | delete,
    id :: m_rsc:resource_id(),
    pre_is_a :: list(),
    post_is_a :: list(),
    pre_props :: m_rsc:props(),
    post_props :: m_rsc:props()
}).

%% @doc Upload and replace the resource with the given data. The data is in the given format.
%% Type: first
%% Return: {ok, Id} or {error, Reason}, return {error, badarg} when the data is corrupt.
-record(rsc_upload, {
    id :: m_rsc:resource() | undefined,
    format :: json | bert,
    data :: binary() | map()
}).

%% @doc Add custom pivot fields to a resource's search index (map)
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
-record(custom_pivot, {
    id :: m_rsc:resource_id()
}).

%% @doc Fold over the resource props map to extend/remove data to be pivoted
%% Type: foldl
-record(pivot_rsc_data, {
    id :: m_rsc:resource_id()
}).

%% @doc Pivot just before a m_rsc_update update. Used to pivot fields before the pivot itself.
%% Type: foldr
-record(pivot_update, {
    id :: m_rsc:resource_id(),
    raw_props :: m_rsc:props()
}).

%% @doc Foldr to change or add pivot fields for the main pivot table.
%% The rsc contains all rsc properties for this resource, including pivot properties.
%% Fold with a map containing the pivot fields.
%% Type: foldl
-record(pivot_fields, {
    id :: m_rsc:resource_id(),
    raw_props :: m_rsc:props()
}).

%% @doc Signal that a resource pivot has been done.
%% Type: notify
-record(rsc_pivot_done, {
    id :: m_rsc:resource_id(),
    is_a = [] :: list( atom() )
}).


%% @doc Sanitize an HTML element.
%% Type: foldl
-record(sanitize_element, {
    element :: {binary(), list( {binary(), binary()} ), list()},
    stack :: list()
}).

%% @doc Sanitize an embed url. The hostpart is of the format: ``<<"youtube.com/v...">>``.
%% Type: first
%% Return: ``undefined``, ``false`` or a binary with a acceptable hostpath
-record(sanitize_embed_url, {
    hostpath :: binary()
}).


%% @doc Check if a user is the owner of a resource.
%% ``id`` is the resource id.
%% Type: first
%% Return: ``true``, ``false`` or ``undefined`` to let the next observer decide
-record(acl_is_owner, {
    id :: m_rsc:resource_id(),
    creator_id :: m_rsc:resource_id(),
    user_id :: m_rsc:resource_id()
}).

%% @doc Check if a user is authorized to perform an operation on a an object
%% (some resource or module). Observe this notification to do complex or more
%% fine-grained authorization checks than you can do through the ACL rules admin
%% interface. Defaults to ``false``.
%% Type: first
%% Return: ``true`` to allow the operation, ``false`` to deny it or ``undefined`` to let the next observer decide
-record(acl_is_allowed, {
    action :: view | update | delete | insert | use | atom(),
    object :: term()
}).

%% @doc Check if a user is authorizded to perform an action on a property.
%% Defaults to ``true``.
%% Type: first
%% Return: ``true`` to grant access, ``false`` to deny it, ``undefined`` to let the next observer decide
-record(acl_is_allowed_prop, {
    action :: view | update | delete | insert | atom(),
    object :: term(),
    prop :: binary()
}).

%% @doc Set the context to a typical authenticated user. Used by m_acl.erl
%% Type: first
%% Return: authenticated ``#context{}`` or ``undefined``
-record(acl_context_authenticated, {
}).

%% @doc Initialize context with the access policy for the user.
%% Type: first
%% Return: updated ``z:context()`` or ``undefined``
-record(acl_logon, {
    id :: m_rsc:resource_id(),
    options :: map()
}).

%% @doc Clear the associated access policy for the context.
%% Type: first
%% Return: updated ``z:context()`` or ``undefined``
-record(acl_logoff, {}).

%% @doc Return the groups for the current user.
%% Type: first
%% Return: ``[ m_rsc:resource_id() ]`` or ``undefined``
-record(acl_user_groups, {}).

%% @doc Modify the list of user groups of a user. Called internally
%% by the ACL modules when fetching the list of user groups a user
%% is member of.
%% Type: foldl
%% Return: ``[ m_rsc:resource_id() ]``
-record(acl_user_groups_modify, {
    id :: m_rsc:resource_id() | undefined,
    groups :: list( m_rsc:resource_id() )
}).

%% @doc Modify the list of collaboration groups of a user. Called internally
%% by the ACL modules when fetching the list of collaboration groups a user
%% is member of.
%% Type: foldl
%% Return: ``[ m_rsc:resource_id() ]``
-record(acl_collab_groups_modify, {
    id :: m_rsc:resource_id() | undefined,
    groups :: list( m_rsc:resource_id() )
}).

%% @doc Confirm a user id.
%% Type: foldl
%% Return: ``z:context()``
-record(auth_confirm, {
    id :: m_rsc:resource_id()
}).

%% @doc A user id has been confirmed.
%% Type: notify
-record(auth_confirm_done, {
    id :: m_rsc:resource_id()
}).

%% @doc First for logon of user with username, check for ratelimit, blocks etc.
%% Return: 'undefined' | ok | {error, Reason}
-record(auth_precheck, {
        username :: binary()
    }).

%% @doc First for logon of user with username, called after successful password check.
%% Return: 'undefined' | ok | {error, Reason}
-record(auth_postcheck, {
        service = username_pw :: atom(),
        id :: m_rsc:resource_id(),
        query_args = #{} :: map()
    }).

%% @doc Fold over the context after logon of user with username, communicates valid or invalid password
-record(auth_checked, {
        id :: undefined | m_rsc:resource_id(),
        username :: binary(),
        is_accepted :: boolean()
    }).

%% @doc First to check for password reset forms, return undefined, ok, or {error, Reason}.
-record(auth_reset, {
        username :: undefined | binary()
    }).

%% @doc First to validate a password. Return {ok, RscId} or {error, Reason}.
-record(auth_validate, {
        username :: undefined | binary(),
        password :: undefined | binary()
    }).


%% @doc User logs on. Add user-related properties to the logon request context.
%% Type: foldl
%% Return: ``z:context()``
-record(auth_logon, { id :: m_rsc:resource_id() }).


%% @doc User is about to log off. Modify (if needed) the logoff request context.
%% Type: foldl
%% Return: ``z:context()``
-record(auth_logoff, { id :: m_rsc:resource_id() | undefined }).


%% @doc Authentication against some (external or internal) service was validated
%% Type: first
-record(auth_validated, {
    service :: atom(),
    service_uid :: binary(),
    service_props = #{} :: map(),
    props = #{} :: m_rsc:props(),
    identities = [] :: list( map() ),
    ensure_username_pw = true :: boolean(),
    is_connect = false :: boolean(),
    is_signup_confirmed = false :: boolean()
}).

%% @doc Update the given (accumulator) authentication options with the request options.
%%      Note that the request options are from the client and are unsafe.
%% Type: foldl
%% Return: ``map()``
-record(auth_options_update, {
        request_options = #{} :: map()
    }).

%% @doc Send a request to the client to login a user. The zotonic.auth.worker.js will
%%      send a request to controller_authentication to exchange the one time token with
%%      a z.auth cookie for the given user. The client will redirect to the Url.
%% Type: first
%% Return: ``ok | {error, term()}``
-record(auth_client_logon_user, {
        user_id :: m_rsc:resource_id(),
        url = <<"#reload">> :: binary() | undefined
    }).

%% @doc Send a request to the client to switch users. The zotonic.auth.worker.js will
%%      send a request to controller_authentication to perform the switch.
%% Type: first
%% Return: ``ok | {error, term()}``
-record(auth_client_switch_user, {
        user_id :: m_rsc:resource_id()
    }).

%% @doc Return the list of identity types that allow somebody to logon and become an
%% active user of the system. Defaults to [ username_pw ].  In the future more types
%% can be requested, think of 'contact' - to be able to contact someone.
%% Type: foldl
%% Return: ``[ atom() ]``
-record(auth_identity_types, {
        type = user :: user
    }).

%% @doc Called during different moments of the request.
%%      * init - called on every http request
%%      * refresh - called after init and on mqtt context updates
%%      * auth_status - called on every authentication status poll
%% Type: foldl
%% Return: ``z:context()``
-record(request_context, {
        phase = init :: init | refresh | auth_status,

        % Document properties from the auth_status call. The client adds
        % here properties like the client's preferred language and timezone.
        document = #{} :: map()
    }).

%% @doc Refresh the context or request process for the given request or action
%%      Called for every request that is not anonymous and before every MQTT relay from
%%      the client.  Example: mod_development uses this to set flags in the process
%%      dictionary.
%% Type: foldl
%% Return: ``#context{}``
-record(session_context, {
        request_type :: http | mqtt,
        payload = undefined :: undefined | term()
    }).

%% @doc Called just before validation of all query arguments by z_validation.
%%      This is the moment to filter any illegal arguments or change query
%%      arguments.
%% Type: foldl
%% Return: ``{ok, list( {binary(), z:qvalue()} )} | {error, term()}``
-record(validate_query_args, {}).

%% @doc Check if a user is enabled. Enabled users are allowed to log in.
%% Type: first
%% Return ``true``, ``false`` or ``undefined``. If ``undefined`` is returned,
%% the user is considered enabled if the user resource is published.
-record(user_is_enabled, { id :: m_rsc:resource_id() }).

%% @doc Set #context fields depending on the user and/or the preferences of the user.
%% Type: foldl
-record(user_context, { id :: m_rsc:resource_id() }).

%% @doc Request API logon
-record(service_authorize, { service_module }).

%% @doc Fetch the url of a resource's html representation
%% Type: first
%% Return: ``{ok, Url}`` or ``undefined``
-record(page_url, { id :: m_rsc:resource_id(), is_a :: list(atom()) }).

%% @doc Handle custom named search queries in your function.
%% Type: first
%% Return: ``#search_sql{}``, ``#search_result{}`` or ``undefined``
-record(search_query, {
    name = undefined :: binary() | undefined,
    args = undefined :: map() | undefined,
    offsetlimit :: {
        Offset :: pos_integer(),
        Limit :: pos_integer()
    },
    options = #{} :: z_search:search_options(),
    % Deprecated {searchname, [..]} syntax.
    search = undefined :: {
        SearchName :: atom(),
        SearchProps :: list()
    } | undefined
}).

%% @doc Map a custom search term to a ``#search_sql_term{}`` record.
%% Type: first
%% Return: ``#search_sql_term{}``, ``[]``, or ``undefined``
-record(search_query_term, {
    term :: binary(),
    arg :: any()
}).

%% @doc An edge has been inserted.
%% Note that the Context for this notification does not have the user who
%% created the edge.
%% Type: notify
%% Return: return value is ignored
-record(edge_insert, {
    subject_id :: m_rsc:resource(),
    predicate :: atom(),
    object_id :: m_rsc:resource(),
    edge_id :: pos_integer()
}).

%% @doc An edge has been deleted
%% Note that the Context for this notification does not have the user who
%% deleted the edge.
%% Type: notify
%% Return: return value is ignored
-record(edge_delete, {
    subject_id :: m_rsc:resource(),
    predicate :: atom(),
    object_id :: m_rsc:resource(),
    edge_id :: pos_integer()
}).

%% @doc An edge has been updated
%% Note that the Context for this notification does not have the user who
%% updated the edge.
%% Type: notify
%% Return: return value is ignored
-record(edge_update, {
    subject_id :: m_rsc:resource(),
    predicate :: atom(),
    object_id :: m_rsc:resource(),
    edge_id :: pos_integer()
}).

%% @doc Site configuration parameter was changed
%% Type: notify
%% Return: return value is ignored
-record(m_config_update, {
    module :: atom(),
    key :: term(),
    value :: term()
}).

%% @doc Site configuration parameter was changed
%% Type: notify
%% Return: return value is ignored
-record(m_config_update_prop, {module, key, prop, value}).


%% @doc Fetch the data for an import of a resource. Returns data in the format
%% used by m_rsc_export and m_rsc_import. Either returns the JSON data, the
%% imported resource id, or the resource id and a map with a mapping from URIs to
%% resource ids.
%% Type: first
%% Return: {ok, map()} | {ok, m_rsc:resource_id()} | {ok, {m_rsc:resource_id(), map()}} | {error, term()} | undefined
-record(rsc_import_fetch, {
    uri :: binary()
}).


%% @doc Notification for fetching #media_import_props{} from different modules.
%% This is used by z_media_import.erl for fetching properties and medium information (map)
%% about resources.  The metadata is the result returned by z_url_metadata.
%% Type: map
-record(media_import, {
    url :: binary(),
    host_rev :: list(binary()),
    mime :: binary(),
    metadata :: tuple() % z_url_metadata:url_metadata()
}).

% Return value of the media_import notification
-record(media_import_props, {
    prio = 5 :: pos_integer(),      % 1 for perfect match (ie. host specific importer)
    category :: atom(),
    module :: atom(),
    description :: binary() | z:trans(),
    rsc_props :: map(),
    medium_props :: z_media_identify:media_info(),
    medium_url = <<>> :: binary(),
    preview_url :: binary() | undefined,
    importer :: atom()
}).

%% @doc Notification to translate or map a file after upload, before insertion into the database
%% Used in mod_video to queue movies for conversion to mp4.
%% You can set the post_insert_fun to something like fun(Id, Medium, Context) to receive the
%% medium record as it is inserted.
%% Type: first
%% Return: modified ``#media_upload_preprocess{}``
-record(media_upload_preprocess, {
    id = insert_rsc :: m_rsc:resource_id() | insert_rsc,
    mime :: binary(),
    file :: file:filename_all() | undefined,
    original_filename :: file:filename_all() | undefined,
    medium :: z_media_identify:media_info(),
    post_insert_fun :: function() | undefined
}).

%% @doc Notification that a medium file has been uploaded.
%% This is the moment to change properties, modify the file etc.
%% The folded accumulator is the map with updated medium properties.
%% Type: foldl
%% Return: modified medium properties map
-record(media_upload_props, {
    id :: m_rsc:resource_id() | insert_rsc,
    mime :: binary(),
    archive_file :: file:filename_all() | undefined,
    options :: list()
}).

%% @doc Notification that a medium file has been uploaded.
%% This is the moment to change resource properties, modify the file etc.
%% The folded accumulator is the map with updated resource properties.
%% Type: foldl
%% Return: modified resource properties map
-record(media_upload_rsc_props, {
    id :: m_rsc:resource_id() | insert_rsc,
    mime :: binary(),
    archive_file,
    options :: list(),
    medium :: z_media_identify:media_info()
}).

%% @doc Notification to import a medium record from external source. This is called for non-file
%% medium records, for example embedded video.  If the medium record is not recognized then it
%% will not be imported. The handling module is responsible for sanitizing and inserting the medium
%% record.
%% Type: first
%% Return: ``ok | {error, term()}``.
-record(media_import_medium, {
    id :: m_rsc:resource_id(),
    medium :: map()
}).

%% @doc Notification that a medium file has been changed (notify)
%% The id is the resource id, medium contains the medium's property list.
%% Type: notify
%% Return: return value is ignored
-record(media_replace_file, {id, medium}).

%% @doc Media update done notification. action is 'insert', 'update' or 'delete'
%% Type: notify
-record(media_update_done, {
    action :: insert | update | delete,
    id :: m_rsc:resource_id(),
    pre_is_a :: list( atom() ),
    post_is_a :: list( atom() ),
    pre_props :: map() | undefined,
    post_props :: map() | undefined
}).

%% @doc Modify the options for an image preview url or tag. This is called for every
%% image url generation, except if the 'original' image option is passed. The mediaclass
%% in the options is not yet expanded.
%% Type: foldl
%% Return: modified property list of image options
-record(media_preview_options, {
    id :: m_rsc:resource_id() | undefined,
    width :: non_neg_integer(),
    height :: non_neg_integer(),
    options :: proplists:proplist()
    }).

%% @doc Request a translation of a list of strings. The resulting translations must
%% be in the same order as the request. This notification is handled by modules
%% that interface to external translation services like DeepL or Google Translate.
%% Type: first
%% Return {ok, List} | {error, Reason} | undefined.
-record(translate, {
    from :: atom(),
    to :: atom(),
    texts = [] :: list( binary() )
    }).

%% @doc Try to detect the language of a translation. Set is_editable_only to false
%% to detect any language, even if the language is not enabled for the site.
%% Type: first
%% Return atom() | undefined.
-record(language_detect, {
    text = <<>> :: binary(),
    is_editable_only = true :: boolean()
    }).

%% @doc Send a notification that the resource 'id' is added to the query query_id.
%% Type: notify
%% Return: return value is ignored
-record(rsc_query_item, {
    query_id :: m_rsc:resource_id(),
    match_id :: m_rsc:resource_id()
}).


%% @doc Add extra javascript with the {% script %} tag. (map)
%% Used to let modules inject extra javascript depending on the arguments of the {% script %} tag.
%% Must return an iolist()
%% Type: map
-record(scomp_script_render, {
    is_nostartup = false :: boolean(),
    args = [] :: list()
}).


%% @doc Render the javascript for a custom action event type.
%% The custom event type must be a tuple, for example:
%% ``{% wire type={live id=myid} action={...} %}</code>``
%% Must return {ok, Javascript, Context}
%% Type: first
-record(action_event_type, {
    event :: tuple(),
    trigger_id :: string(),
    trigger :: string(),
    postback_js :: iolist(),
    postback_pickled :: string()|binary(),
    action_js :: iolist()
}).

%% @doc Find an import definition for a CSV file by checking the filename of the to be imported file.
%% Type: first
%% Return: ``#import_csv_definition{}`` or ``undefined`` (in which case the column headers are used as property names)
-record(import_csv_definition, {
    basename :: binary(),
    filename :: file:filename_all()
}).


%% @doc Handle an uploaded file which is part of a multiple file upload from a user-agent.
%% The upload is a #upload record or a filename on the server.
%% Type: first
%% Return: ``#context{}`` with the result or ``undefined``
-record(multiupload, {
    upload :: term() | string(),
    query_args = [] :: list()
}).

%% @doc Handle a new file received in the 'files/dropbox' folder of a site.
%% Unhandled files are deleted after a hour.
%% Type: first
-record(dropbox_file, {
    filename :: file:filename_all(),
    basename :: binary()
}).

%% @doc Try to identify a file, returning a map with file properties.
%% Type: first
%% Return: map with binary keys, especially ``<<"mime">>``, ``<<"width">>``, ``<<"height">>``, ``<<"orientation">>``
-record(media_identify_file, {
    filename :: file:filename_all(),
    original_filename :: binary(),
    extension :: binary()
}).

%% @doc Try to find a filename extension for a mime type (example: ".jpg")
%% Type: first
%% Return: Extension (for example ``<<".png">>``) or ``undefined``
-record(media_identify_extension, {
    mime :: binary(),
    preferred :: undefined | binary()
}).

%% @doc Request to generate a HTML media viewer for a resource
%% Type: first
%% Return: ``{ok, Html}`` or ``undefined``
-record(media_viewer, {
    id,
    props :: z_media_identify:media_info(),
    filename = undefined :: file:filename_all() | undefined,
    options = [] :: list()
}).

%% @doc See if there is a 'still' image preview of a media item. (eg posterframe of a movie)
%% Type: first
%% Return:: ``{ok, ResourceId}`` or ``undefined``
-record(media_stillimage, {
    id :: m_rsc:resource_id() | undefined,
    props :: z_media_identify:media_info()
}).

%% @doc Optionally wrap HTML with external content so that it adheres to the cookie/privacy
%% settings of the current site visitor. Typically called with a 'first' by the code that
%% generated the media viewer HTML, as that code has the knowledge if viewing the generated code
%% has any privacy or cookie implications.
%% Return {ok, HTML} or undefined
-record(media_viewer_consent, {
    id :: m_rsc:resource_id() | undefined,
    consent = all :: functional | stats | all,
    html :: iodata(),
    viewer_props :: z_media_identify:media_info(),
    viewer_options = [] :: list()
}).

%% @doc Fetch list of handlers for survey submits.
%% Type: foldr
%% Return: list with tuples: ``[ {handler_name, TitleForDisplay}, ... ]``
-record(survey_get_handlers, {}).

%% @doc A survey has been filled in and submitted.
%% Type: first
%% Return: ``undefined``, ``ok``, ``{ok, Context | #render{}}``, ``{save, Context | #render{}`` or ``{error, term()}``
-record(survey_submit, {
    id :: m_rsc:resource_id(),
    handler :: binary() | undefined,
    answers :: list(),
    missing :: list(),
    answers_raw :: list(),
    submit_args :: proplists:proplist()
}).

%% @doc Check if the current user is allowed to download a survey.
%% Type: first
%% Return: ``true``, ``false`` or ``undefined``
-record(survey_is_allowed_results_download, {
    id :: m_rsc:resource_id()
}).

%% @doc Check if a question is a submitting question.
%% Type: first
%% Return: ``true``, ``false`` or ``undefined``
-record(survey_is_submit, {
    block = #{} :: map()
}).

%% @doc Add header columns for export. The values are the names of the answers and
%% the text displayed above the column. The ``text`` format is for a complete export, the
%% ``html`` format is for the limited result overview of the Survey Results Editor.
%% Type: foldl
%% Return: ``list( {binary(), binary() | #trans{}} )``
-record(survey_result_columns, {
    id :: m_rsc:resource_id(),
    handler :: binary() | undefined,
    format :: html | text
}).

%% @doc Modify row with answers for export. The header columns are given and the
%% values that are known are set in the folded value. The user_id is the user who
%% filled in the answers for this row.
%% Type: foldl
%% Return: ``#{ binary() => iodata() }``
-record(survey_result_column_values, {
    id :: m_rsc:resource_id(),
    handler :: binary() | undefined,
    format :: html | text,
    user_id :: m_rsc:resource_id(),
    answer :: proplists:proplist(),
    columns :: list( {binary(), binary() | #trans{}} )
}).

%% @doc Put a value into the typed key/value store
%% Type: notify
-record(tkvstore_put, {type, key, value}).

%% @doc Get a value from the typed key/value store
%% Type: first
-record(tkvstore_get, {type, key}).

%% @doc Delete a value from the typed key/value store
%% Type: notify
%% Return: return value is ignored
-record(tkvstore_delete, {type, key}).

%% @doc Internal message of mod_development. Start a stream with debug information to the user agent.
%% 'target' is the id of the HTML element where the information is inserted.
%% 'what' is the kind of debug information being streamed to the user-agent.
-record(debug_stream, {target, what = template}).

%% @doc Push some information to the debug page in the user-agent.
%% Will be displayed with io_lib:format("~p: ~p~n", [What, Arg]), be careful with escaping information!
-record(debug, {what, arg = []}).

%% @doc Broadcast some file changed, used for livereload by mod_development
%% Type: notify
%% Return: return value is ignored
-record(filewatcher, {
    verb :: modify | create | delete,
    file :: binary(),
    basename :: binary(),
    extension :: binary()
}).

%% @doc An external feed delivered a resource. First handler can import it.
%% Type: first
%% Return:: ``{ok, m_rsc:resource_id()}``, ``{error, Reason}``, or ``undefined``
-record(import_resource, {
    source :: atom() | binary(),
    source_id :: integer() | binary(),
    source_url :: binary(),
    source_user_id :: binary() | integer(),
    user_id :: integer(),
    name :: binary(),
    props :: m_rsc:props_all(),
    urls :: list(),
    media_urls :: list(),
    data :: any()
}).

%% @doc mod_export - return the {ok, Disposition} for the content disposition.
%% Type: first
%% Return: {ok, <<"inline">>} or {ok, <<"attachment">>}
-record(export_resource_content_disposition, {
    dispatch :: atom(),
    id :: m_rsc:resource_id() | undefined,
    content_type :: binary()
}).

%% @doc mod_export - Check if the resource or dispatch is visible for export.
%% Type: first
%% Return: ``true`` or ``false``
-record(export_resource_visible, {
    dispatch :: atom(),
    id :: m_rsc:resource_id() | undefined
}).

%% @doc mod_export -
%% Type: first
%% Return: ``{ok, "text/csv"})`` for the dispatch rule/id export.
-record(export_resource_content_type, {
    dispatch :: atom(),
    id :: m_rsc:resource_id() | undefined
}).

%% @doc mod_export - return the {ok, Filename} for the content disposition.
%% Type: first
%% Return: ``{ok, Filename}}`` or ``undefined``
-record(export_resource_filename, {
    dispatch :: atom(),
    id :: m_rsc:resource_id() | undefined,
    content_type :: binary()
}).

%% @doc mod_export - Fetch the header for the export.
%% Type: first
%% Return: ``{ok, list()|binary()}``, ``{ok, list()|binary(), ContinuationState}`` or ``{error, Reason}``
-record(export_resource_header, {
    dispatch :: atom(),
    id :: m_rsc:resource_id() | undefined,
    content_type :: binary()
}).

%% @doc mod_export - fetch a row for the export, can return a list of rows, a binary, and optionally a continuation state.
%% Where Values is [ term() ], i.e. a list of opaque values, to be formatted with #export_resource_format.
%% Return the empty list of values to signify the end of the data stream.
%% Type: first
%% Return: ``{ok, Values|binary()}``, ``{ok, Values|binary(), ContinuationState}`` or ``{error, Reason}``
-record(export_resource_data, {
    dispatch :: atom(),
    id :: m_rsc:resource_id() | undefined,
    content_type :: binary(),
    state :: term()
}).

%% @doc mod_export - Encode a single data element.
%% Type: first
%% Return: ``{ok, binary()}``, ``{ok, binary(), ContinuationState}`` or ``{error, Reason}``
-record(export_resource_encode, {
    dispatch :: atom(),
    id :: m_rsc:resource_id() | undefined,
    content_type :: binary(),
    data :: term(),
    state :: term()
}).

%% @doc mod_export - Fetch the footer for the export. Should cleanup the continuation state, if needed.
%% Type: first
%% Return: ``{ok, binary()}`` or ``{error, Reason}``
-record(export_resource_footer, {
    dispatch :: atom(),
    id :: m_rsc:resource_id() | undefined,
    content_type :: binary(),
    state :: term()
}).

%% @doc Handle a javascript notification from the postback handler. The ``message`` is the the request,
%% ``trigger`` the id of the element which triggered the postback, and ``target`` the
%% id of the element which should receive possible updates. ``#postback_notify`` is also used as an event.
%% Type: first
%% Return: ``undefined`` or ``#context{}`` with the result of the postback
-record(postback_notify, {
    message,
    trigger,
    target,
    data
}).

%% @doc Message sent by a user-agent on a postback event. Encapsulates the encoded postback and any
%% additional data. This is handled by z_transport.erl, which will call the correct event/2 functions.
%% Type: first
-record(postback_event, {
    postback,
    trigger,
    target,
    triggervalue,
    data
}).


%% @doc Determine the URL fetch options for fetching the content of an URL. Used by z_fetch.erl.
%% Type: first
%% Return: ``z_url_fetch:options()``
-record(url_fetch_options, {
    method :: get | post | put | delete,
    host :: binary(),
    url :: binary(),
    options :: z_url_fetch:options()
}).


%% @doc Delegates the request processing.
%% Type: foldl
%% Return: updated ``z:context()``
-record(middleware, {
    on :: request | welformed | handled
}).


% Simple mod_development notifications:
% development_reload - Reload all template, modules etc
% development_make - Perform a 'make' on Zotonic, reload all new beam files

