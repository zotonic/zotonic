%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2017 Marc Worrell
%% @doc Notifications used in Zotonic core

%% Copyright 2011-2017 Marc Worrell
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
    tracer_pid = undefined :: atom() | pid()
}).

-record(dispatch_redirect, {
    location = <<>> :: binary(),
    is_permanent = false :: boolean()
}).

-record(dispatch_match, {
    dispatch_name = undefined :: atom(),
    mod :: atom(),
    mod_opts = [] :: list(),
    path_tokens = [] :: list(binary()),
    bindings = [] :: list({atom(), binary()})
}).

-record(dispatch_rules, {
    rules :: #site_dispatch_list{} | undefined
}).


%% @doc Modify cookie options, used for setting http_only and secure options. (foldl)
-record(cookie_options, {name, value}).

% 'module_ready' - Sent when modules have changed, z_module_indexer reindexes all modules' templates, actions etc.

%% @doc A module has been activated and started. (notify)
-record(module_activate, {
    module :: atom(),
    pid :: pid()
}).

%% @doc A module has been stopped and deactivated. (notify)
-record(module_deactivate, {
    module :: atom()
}).


%% @doc Possibility to overrule a property of a resource (currently only the title)
-record(rsc_property, {
    id :: m_rsc:resource(),
    property :: atom(),
    value :: term()
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
    request_page = [] :: string()
}).

%% @doc Determine post-logon actions; args are the arguments passed to the logon
%% submit wire
-record(logon_actions, {
    args = [] :: list()
}).

%% @doc Handle a user logon. The posted query args are included.
%% Type: first
%% Return:: ``{ok, UserId}`` or ``{error, Reason}``
-record(logon_submit, {
    query_args = [] :: list()
}).

%% @doc Request to send a verification to the user. Return ok or an error
%% Type: first
%% Identity may be undefined, or is a identity used for the verification.
-record(identity_verification, {user_id, identity}).

%% @doc Notification that a user's identity has been verified.
%% Type: notify
-record(identity_verified, {user_id, type, key}).

-record(identity_password_match, {rsc_id, password, hash}).


%% @doc Handle a signup of a user, return the follow on page for after the signup.
%% Type: first
%% Return ``{ok, Url}``
%% 'props' is a proplist with properties for the person resource (email, name, etc)
%% 'signup_props' is a proplist with 'identity' definitions and optional follow on url 'ready_page'
%% An identity definition is {Kind, Identifier, IsUnique, IsVerified}
-record(signup_url, {
    props = [] :: list(),
    signup_props = [] :: list()
}).

%% @doc Request a signup of a new or existing user. Arguments are similar to #signup_url{}
%% Returns {ok, UserId} or {error, Reason}
-record(signup, {
    id :: integer(),
    props = [] :: list(),
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
    props = [] :: list(),
    signup_props = [] :: list()
}).

%% @doc Signal that a user has been signed up (map, result is ignored)
-record(signup_done, {
    id :: m_rsc:resource(),
    is_verified :: boolean(),
    props :: list(),
    signup_props :: list()
}).

%% @doc Signal that a user has been confirmed. (map, result is ignored)
-record(signup_confirm, {
    id :: m_rsc:resource()
}).

%% @doc Fetch the page a user is redirected to after signing up with a confirmed identity (first)
%% Return: a URL or ``undefined``
-record(signup_confirm_redirect, {
    id :: m_rsc:resource()
}).


%% @doc Handle a javascript notification from the postback handler. The 'message' is the z_msg argument of
%% the request. (first), 'trigger' the id of the element which triggered the postback, and 'target' the
%% id of the element which should receive possible updates. Note: postback_notify is also used as an event.
%% Return: ``undefined`` or ``#context{}`` with the result of the postback
-record(postback_notify, {message, trigger, target, data}).

%% @doc Message sent by a user-agent on a postback event. Encapsulates the encoded postback and any
%% additional data. This is handled by z_transport.erl, which will call the correct event/2 functions.
-record(postback_event, {postback, trigger, target, triggervalue, data}).

%% @doc Notification to signal an inserted comment.
%% 'comment_id' is the id of the inserted comment, 'id' is the id of the resource commented on.
%% Type: notify
-record(comment_insert, {comment_id, id}).

%% @doc Notify that the session's language has been changed
%% Type: notify
-record(language, {language}).

%% @doc Set the language of the context to a user's prefered language
%% Type: first
-record(set_user_language, {id}).

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
-record(admin_edit_blocks, {id}).

%% @doc Used in the admin to process a submitted resource form
-record(admin_rscform, {id, is_a}).

%% @doc Used for fetching the menu in the admin.
%% Type: foldl
%% Return: list of admin menu items
-record(admin_menu, {}).

%% @doc Fetch the menu id belonging to a certain resource
%% Type: first
-record(menu_rsc, {
    id :: m_rsc:resource()
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


%% @doc e-mail notification used by z_email and z_email_server.
-record(email, {
    to = [] :: list(),
    cc = [] :: list(),
    bcc = [] :: list(),
    from = [] :: list(),
    reply_to,
    headers = [] :: list(),
    body,
    raw,
    subject,
    text,
    html,
    text_tpl,
    html_tpl,
    vars = [] :: list(),
    attachments = [] :: list(),
    queue = false :: boolean()
}).

%% @doc Notification sent to a site when e-mail for that site is received
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

%% @doc Email status notification, sent when the validity of an email recipient changes
%% Type: notify
-record(email_status, {
    recipient :: binary(),
    is_valid :: boolean(),
    is_final :: boolean()
}).

%% @doc Bounced e-mail notification.  The recipient is the e-mail that is bouncing. When the
%% the message_nr is unknown the it is set to 'undefined'. This can happen if it is a "late bounce".
%% If the recipient is defined then the Context is the depickled z_email:send/2 context.
%% (notify)
-record(email_bounced, {
    message_nr :: binary(),
    recipient :: undefined | binary()
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
    reason :: retry | illegal_address | smtphost | error,
    retry_ct :: non_neg_integer(),
    status :: binary()
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
-record(mailinglist_mailing, {list_id, page_id}).

%% @doc Send a welcome or goodbye message to the given recipient. (notify).
%% The recipient is either an e-mail address or a resource id.
%% 'what' is send_welcome, send_confirm, send_goobye or silent.
-record(mailinglist_message, {what, list_id, recipient}).

%% @doc Save (and update) the complete category hierarchy (notify)
-record(category_hierarchy_save, {tree}).

%% @doc Save the menu tree of a menu resource (notify)
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
-record(rsc_get, {id}).

%% @doc Resource will be deleted.
%% This notification is part of the delete transaction, it's purpose is to clean up
%% associated data.
%% Type: notify
-record(rsc_delete, {id, is_a}).

%% @doc Foldr for an resource insert, these are the initial properties and will overrule
%% the properties in the insert request. Use with care.  The props are the properties of
%% the later insert, after escaping/filtering but before the #rsc_update{} notification below.
%% Type: foldr
%% Return: proplist accumulator
-record(rsc_insert, {
    props :: list()
}).

%% @doc Map to signal merging two resources. Move any information from the loser to the
%% winner. The loser will be deleted.
-record(rsc_merge, {
    winner_id :: integer(),
    loser_id :: integer()
}).

%% @doc An updated resource is about to be persisted.
%% Observe this notification to change the resource properties before they are
%% persisted.
%% The props are the resource's props _before_ the update.
%% The folded value is {IsChanged, UpdateProps} for the update itself.
%% Set IsChanged to true if you modify the UpdateProps.
%% Type: foldr
%% Return: ``{true, ChangedProps}`` or ``{false, Props}``
-record(rsc_update, {
    action :: insert | update,
    id :: m_rsc:resource(),
    props :: list()
}).

%% @doc An updated resource has just been persisted. Observe this notification to
%% execute follow-up actions for a resource update.
%% Type: notify
%% Return: return value is ignored
-record(rsc_update_done, {
    action :: insert | update | delete,
    id :: m_rsc:resource(),
    pre_is_a :: list(),
    post_is_a :: list(),
    pre_props :: list(),
    post_props :: list()
}).

%% @doc Upload and replace the the resource with the given data. The data is in the given format.
%% Return {ok, Id} or {error, Reason}, return {error, badarg} when the data is corrupt.
-record(rsc_upload, {id, format :: json|bert, data}).

%% @doc Add custom pivot fields to a resource's search index (map)
%% Result is a list of {module, props} pairs.
%% This will update a table "pivot_<module>".
%% You must ensure that the table exists.
%% Type: map
-record(custom_pivot, {
    id :: m_rsc:resource()
}).

% 'pivot_rsc_data' - foldl over the resource props to extend/remove data to be pivoted

%% @doc Pivot just before a m_rsc_update update. Used to pivot fields before the pivot itself.
%% Type: foldr
-record(pivot_update, {
    id :: m_rsc:resource(),
    raw_props :: list()
}).

%% @doc Foldr to change or add pivot fields for the main pivot table.
%%  The rsc contains all rsc properties for this resource, including pivot properties.
-record(pivot_fields, {
    id :: m_rsc:resource(),
    rsc :: list()
}).

%% @doc Signal that a resource pivot has been done.
%% Type: notify
-record(rsc_pivot_done, {
    id :: m_rsc:resource(),
    is_a = [] :: list()
}).


%% @doc Sanitize an HTML element.
%% Type: foldl
-record(sanitize_element, {
    element :: {binary(), list(), list()},
    stack :: list()
}).

%% @doc Sanitize an embed url. The hostpart is of the format: <<"youtube.com/v...">>.
%% Return: ``undefined``, ``false`` or a binary with a acceptable hostpath
-record(sanitize_embed_url, {
    hostpath :: binary()
}).


%% @doc Check if a user is the owner of a resource.
%% ``id`` is the resource id.
%% Type: first
%% Return: ``true``, ``false`` or ``undefined`` to let the next observer decide
-record(acl_is_owner, {
    id :: integer(),
    creator_id :: integer(),
    user_id :: integer()
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
    prop :: atom()
}).

%% @doc Set the context to a typical authenticated uses. Used by m_acl.erl
%% Type: first
%% Return: authenticated ``#context{}`` or ``undefined``
-record(acl_context_authenticated, {

}).

%% @doc Initialize context with the access policy for the user.
%% Type: first
%% Return: updated ``#context`` or ``undefined``
-record(acl_logon, {id}).

%% @doc Clear the associated access policy for the context.
%% Type: first
%% Return: updated ``#context{}`` or ``undefined``
-record(acl_logoff, {}).

%% @doc Confirm a user id.
%% Type: foldl
%% Return: ``context{}``
-record(auth_confirm, {}).

%% @doc A user id has been confirmed.
%% Type: notify
-record(auth_confirm_done, {}).

%% @doc User logs on. Add user-related properties to the session.
%% Type: foldl
%% Return: ``context{}``
-record(auth_logon, {}).

%% @doc User has logged on.
%% Type: notify
-record(auth_logon_done, {}).

%% @doc User is about to log off. Remove authentication from the current session.
%% Type: foldl
%% Return: ``context{}``
-record(auth_logoff, {}).

%% @doc User has logged off.
%% Type: notify
-record(auth_logoff_done, {}).

%% @doc Check if automatic logon is enabled for this session. Sent for new
%% sessions from ``z_auth:logon_from_session/1``. Please note this notification
%% is sent for every single request.
%% Type: first
%% Return: ``{ok, UserId}`` when a user should be logged on.
-record(auth_autologon, {}).

%% @doc Authentication against some (external or internal) service was validated
-record(auth_validated, {
    service :: atom(),
    service_uid :: binary(),
    service_props = [] :: list(),
    props = [] :: list({atom(), any()}),
    is_connect = false :: boolean(),
    is_signup_confirm = false :: boolean()
}).

%% @doc Called after parsing the query arguments
%% Type: foldl
%% Return: ``#context{}``
-record(request_context, {}).

%% @doc Initialize a context from the current session.
%% Called for every request that has a session.
%% Type: foldl
%% Return: ``#context{}``
-record(session_context, {}).

%% @doc A new session has been intialized: session_pid is in the context.
%% Called for every request that has a session.
%% Type: notify
%% Return: ``#context{}``
-record(session_init, {}).

%% @doc Foldl over the context containing a new session.
%% Called for every request that has a session.
%% Type: foldl
%% Return: ``#context{}``
-record(session_init_fold, {}).

%% @doc Check if a user is enabled. Enabled users are allowed to log in.
%% Type: first
%% Return ``true``, ``false`` or ``undefined``. If ``undefined`` is returned,
%% the user is considered enabled if the user resource is published.
-record(user_is_enabled, {id}).

%% @doc Set #context fields depending on the user and/or the preferences of the user.
%% Type: foldl
-record(user_context, {id}).

%% @doc Request API logon
-record(service_authorize, {service_module}).

%% @doc Fetch the url of a resource's html representation
%% Type: first
%% Return: ``{ok, Url}`` or ``undefined``
-record(page_url, {id, is_a}).

%% @doc Handle custom named search queries in your function.
%% Type: first
%% Return: ``#search_sql{}``, ``#search_result{}`` or ``undefined``
-record(search_query, {
    search :: {
        SearchName :: atom(),
        SearchProps :: list()
    },
    offsetlimit :: {
        Offset :: pos_integer(),
        Limit :: pos_integer()
    }
}).

%% @doc An edge has been inserted
%% Type: notify
%% Return: return value is ignored
-record(edge_insert, {
    subject_id :: m_rsc:resource(),
    predicate :: atom(),
    object_id :: m_rsc:resource(),
    edge_id :: pos_integer()
}).

%% @doc An edge has been deleted
%% Type: notify
%% Return: return value is ignored
-record(edge_delete, {
    subject_id :: m_rsc:resource(),
    predicate :: atom(),
    object_id :: m_rsc:resource(),
    edge_id :: pos_integer()
}).

%% @doc An edge has been updated
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

%% @doc Notification for fetching #media_import_props{} from different modules.
%% This is used by z_media_import.erl for fetching properties and medium information
%% about resources.
-record(media_import, {
    url :: binary(),
    host_rev :: list(binary()),
    mime :: binary(),
    metadata :: tuple()
}).

-record(media_import_props, {
    prio = 5 :: pos_integer(),      % 1 for perfect match (ie. host specific importer)
    category :: atom(),
    module :: atom(),
    description :: binary() | {trans, list()},
    rsc_props :: list(),
    medium_props :: list(),
    medium_url = <<>> :: binary(),
    preview_url :: binary()
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
    file :: file:filename() | undefined,
    original_filename :: file:filename() | undefined,
    medium :: list(),
    post_insert_fun :: function() | undefined
}).

%% @doc Notification that a medium file has been uploaded.
%% This is the moment to change properties, modify the file etc.
%% Type: foldl
%% Return: modified ``#media_upload_props{}``
-record(media_upload_props, {
    id :: integer() | 'insert_rsc',
    mime :: binary(),
    archive_file :: file:filename() | undefined,
    options :: list()
}).

%% @doc Notification that a medium file has been uploaded.
%% This is the moment to change resource properties, modify the file etc.
%% Type: foldl
%% Return: modified ``#media_upload_rsc_props{}``
-record(media_upload_rsc_props, {
    id :: integer() | 'insert_rsc',
    mime :: binary(),
    archive_file,
    options :: list(),
    medium :: list()
}).

%% @doc Notification that a medium file has been changed (notify)
%% The id is the resource id, medium contains the medium's property list.
%% Type: notify
%% Return: return value is ignored
-record(media_replace_file, {id, medium}).

%% @doc Media update done notification.
%% action is 'insert', 'update' or 'delete'
-record(media_update_done, {action, id, pre_is_a, post_is_a, pre_props, post_props}).


%% @doc Send a notification that the resource 'id' is added to the query query_id.
%% Type: notify
-record(rsc_query_item, {
    query_id,
    match_id
}).


%% @doc Add extra javascript with the {% script %} tag. (map)
%% Used to let modules inject extra javascript depending on the arguments of the {% script %} tag.
%% Must return an iolist()
-record(scomp_script_render, {
    is_nostartup = false :: boolean(),
    args = [] :: list()
}).


%% @doc Render the javascript for a custom action event type.
%% The custom event type must be a tuple, for example:
%% <code>{% wire type={live id=myid} action={...} %}</code>
%% Must return {ok, Javascript, Context}
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
-record(import_csv_definition, {basename, filename}).


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
-record(dropbox_file, {filename}).

%% @doc Try to identify a file, returning a list of file properties.
%% Type: first
-record(media_identify_file, {filename, original_filename, extension}).

%% @doc Try to find a filename extension for a mime type (example: ".jpg")
%% Type: first
-record(media_identify_extension, {
    mime :: binary(),
    preferred :: undefined | binary()
}).

%% @doc Request to generate a HTML media viewer for a resource
%% Type: first
%% Return: ``{ok, Html}`` or ``undefined``
-record(media_viewer, {
    id,
    props :: list(),
    filename,
    options = [] :: list()
}).

%% @doc See if there is a 'still' image preview of a media item. (eg posterframe of a movie)
%% Return:: ``{ok, ResourceId}`` or ``undefined``
-record(media_stillimage, {id, props = []}).


%% @doc Fetch lisy of handlers.
%% Type: foldr
-record(survey_get_handlers, {}).

%% @doc A survey has been filled in and submitted.
%% Type: first
-record(survey_submit, {id, handler, answers, missing, answers_raw}).

%% @doc Check if the current user is allowed to download a survey.
%% Type: first
-record(survey_is_allowed_results_download, {id}).

%% @doc Check if a question is a submitting question.
%% Type: first
-record(survey_is_submit, {block = []}).

%% @doc Put a value into the typed key/value store
-record(tkvstore_put, {type, key, value}).

%% @doc Get a value from the typed key/value store
-record(tkvstore_get, {type, key}).

%% @doc Delete a value from the typed key/value store
-record(tkvstore_delete, {type, key}).

%% @doc Subscribe a function to an MQTT topic.
%% The function will be called from a temporary process, and must be of the form:
%% m:f(#emqtt_msg{}, A, Context)
-record(mqtt_subscribe, {topic, qos = 0 :: 0 | 1 | 2, mfa}).

%% @doc Unsubscribe a function from an MQTT topic.
%% The MFA _must_ match the one supplied with #mqtt_subscribe{}
-record(mqtt_unsubscribe, {topic, mfa}).

%% @doc MQTT acl check, called via the normal acl notifications.
%% Actions for these checks: subscribe, publish
-record(acl_mqtt, {
    type :: 'wildcard' | 'direct',
    topic :: binary(),
    words :: list(binary() | integer()),
    site :: binary(),
    page_id :: 'undefined' | binary()
}).

%% @doc Broadcast notification.
-record(broadcast, {title = [], message = [], is_html = false, stay = true, type = "error"}).

%% @doc Internal message of mod_development. Start a stream with debug information to the user agent.
%% 'target' is the id of the HTML element where the information is inserted.
%% 'what' is the kind of debug information being streamed to the user-agent.
-record(debug_stream, {target, what = template}).

%% @doc Push some information to the debug page in the user-agent.
%% Will be displayed with io_lib:format("~p: ~p~n", [What, Arg]), be careful with escaping information!
-record(debug, {what, arg = []}).

%% @doc An external feed delivered a resource. First handler can import it.
-record(import_resource, {
    source :: atom() | binary(),
    source_id :: integer() | binary(),
    source_url :: binary(),
    source_user_id :: binary() | integer(),
    user_id :: integer(),
    name :: binary(),
    props :: list(),
    urls :: list(),
    media_urls :: list(),
    data :: any()
}).

%% @doc mod_export - return the {ok, Disposition} for the content disposition.
%% Type: first
%% Return: {ok, <<"inline">>} or {ok, <<"attachment">>}
-record(export_resource_content_disposition, {
    dispatch :: atom(),
    id :: integer(),
    content_type :: string()
}).

%% @doc mod_export - Check if the resource or dispatch is visible for export.
%% Type: first
%% Return: ``true`` or ``false``
-record(export_resource_visible, {
    dispatch :: atom(),
    id :: integer()
}).

%% @doc mod_export -
%% Return: ``{ok, "text/csv"})`` for the dispatch rule/id export.
-record(export_resource_content_type, {
    dispatch :: atom(),
    id :: integer()
}).

%% @doc mod_export - return the {ok, Filename} for the content disposition.
%% Type: first
%% Return: ``{ok, Filename}}`` or ``undefined``
-record(export_resource_filename, {
    dispatch :: atom(),
    id :: integer(),
    content_type :: string()
}).

%% @doc mod_export - Fetch the header for the export.
%% Type: first
%% Return: ``{ok, list()|binary()}``, ``{ok, list()|binary(), ContinuationState}`` or ``{error, Reason}``
-record(export_resource_header, {
    dispatch :: atom(),
    id :: integer(),
    content_type :: string()
}).

%% @doc mod_export - fetch a row for the export, can return a list of rows, a binary, and optionally a continuation state.
%% Where Values is [ term() ], i.e. a list of opaque values, to be formatted with #export_resource_format.
%% Return the empty list of values to signify the end of the data stream.
%% Type: first
%% Return: ``{ok, Values|binary()}``, ``{ok, Values|binary(), ContinuationState}`` or ``{error, Reason}``
-record(export_resource_data, {
    dispatch :: atom(),
    id :: integer(),
    content_type :: string(),
    state :: term()
}).

%% @doc mod_export - Encode a single data element.
%% Type: first
%% Return: ``{ok, binary()}``, ``{ok, binary(), ContinuationState}`` or ``{error, Reason}``
-record(export_resource_encode, {
    dispatch :: atom(),
    id :: integer(),
    content_type :: string(),
    data :: term(),
    state :: term()
}).

%% @doc mod_export - Fetch the footer for the export. Should cleanup the continuation state, if needed.
%% Type: first
%% Return: ``{ok, binary()}`` or ``{error, Reason}``
-record(export_resource_footer, {
    dispatch :: atom(),
    id :: integer(),
    content_type :: string(),
    state :: term()
}).


% Simple mod_development notifications:
% development_reload - Reload all template, modules etc
% development_make - Perform a 'make' on Zotonic, reload all new beam files
