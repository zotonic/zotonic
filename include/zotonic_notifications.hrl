%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Notifications used in Zotonic core

%% Copyright 2011 Marc Worrell
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

%% @doc Final try for dispatch, try to match the request. Called with z_notifier:first/2
%%      Result:   {ok, RscId::integer()} 
%%              | {ok, #dispatch_match{}} 
%%              | {ok, #dispatch_redirect{}}
%%              | undefined.
-record(dispatch, {host, path=[], method='GET', is_ssl=false}).
    
    -record(dispatch_redirect, {location, is_permanent=false}).
    -record(dispatch_match, {dispatch_name, mod, mod_opts=[], path_tokens=[], bindings=[], app_root="", string_path=""}).


% 'module_ready' - Sent when modules have changed, z_module_indexer reindexes all modules' templates, actions etc.

%% @doc A module has been activated and started. (notify)
-record(module_activate, {module, pid}).

%% @doc A module has been stopped and deactivated. (notify)
-record(module_deactivate, {module}).


%% @doc Possibility to overrule a property of a resource (currently only the title)
-record(rsc_property, {id, property, value}).

%% @doc Foldr by resource_id to find all pairs {ContentType, DispatchRule} available.
%% Example: {"text/html", page}
%% A special dispatch rule is 'page_url', which refers to the page_url property of the resource.
-record(content_types_dispatch, {id}).

% @doc Check where to go after a user logs on. Return an URL or undefined (first)
-record(logon_ready_page, {request_page=[]}).

%% @doc Handle an user logon. The posted query args are included. Return {ok, UserId} or {error, Reason} (first)
-record(logon_submit, {query_args=[]}).

%% @doc Request to send a verification to the user. Return ok or an error (first)
%% Identity may be undefined, or is a identity used for the verification.
-record(identity_verification, {user_id, identity}).


%% @doc Handle a signup of an user, return the follow on page for after the signup. (first)
%% Return {ok, Url}
%% 'props' is a proplist with properties for the person resource (email, name, etc)
%% 'signup_props' is a proplist with 'identity' definitions and optional follow on url 'ready_page'
%% An identity definition is {Kind, Identifier, IsUnique, IsVerified}
-record(signup_url, {props=[], signup_props=[]}).

%% @doc Signup failed, give the error page URL. Return {ok, Url} or undefined. (first)
%% Reason is returned by the signup handler for the particular signup method (username, facebook etc)
-record(signup_failed_url, {reason}).

%% signup_check
%% Check if the signup can be handled, a fold over all modules.
%% Fold argument/result is {ok, Props, SignupProps} or {error, Reason}

%% @doc Signal that an user has been signed up (map, result is ignored)
-record(signup_done, {id, is_verified, props, signup_props}).

%% @doc Signal that an user has been confirmed. (map, result is ignored)
-record(signup_confirm, {id}).

%% @doc Fetch the page an user is redirected to after signing up with a confirmed identity (first)
%% Return either undefined or a Url
-record(signup_confirm_redirect, {id}).


%% @doc Handle a javascript notification from the postback handler. The 'message' is the z_msg argument of
%% the request. (first), 'trigger' the id of the element which triggered the postback, and 'target' the 
%% id of the element which should receive possible updates. Note: postback_notify is also used as an event.
%% Return either 'undefined' or a #context with the result of the postback
-record(postback_notify, {message, trigger, target}).

%% @doc Notification to signal an inserted comment. (notify)
%% 'comment_id' is the id of the inserted comment, 'id' is the id of the resource commented on.
-record(comment_insert, {comment_id, id}).

%% @doc Notify that the session's language has been changed (notify)
-record(language, {language}).

%% @doc Set the language of the context to a user's prefered language (first)
-record(set_user_language, {id}).

%% @doc Rewrite an url after it has been generated using the z_dispatcher (foldl)
-record(url_rewrite, {dispatch, args=[]}).

%% @doc Rewrite an url before it will be dispatched using the z_dispatcher (foldl)
-record(dispatch_rewrite, {is_dir=false, path=""}).

%% @doc Used in the admin to fetch the possible blocks for display (foldl)
-record(admin_edit_blocks, {id}).

%% Used for fetching the menu in the admin (foldl)
% admin_menu

%% @doc An activity in Zotonic. When this is handled as a notification then return a list
%% of patterns matching this activity.  These patterns are then used to find interested
%% subscribers. (map)
-record(activity, {version=1, posted_time, actor, verb=post, object, target}).

%% @doc Push a list of activities via a 'channel' (eg 'email') to a recipient. (first)
%% The activities are a list of #activity{} records.
-record(activity_send, {recipient_id, channel, queue, activities=[]}).


%% @doc e-mail notification used by z_email and z_email_server.
-record(email, {to=[], cc=[], bcc=[], from=[], reply_to, 
                headers=[], body, raw,
                subject, text, html, text_tpl, html_tpl, 
                vars=[], attachments=[], queue=false}).

%% @doc Notification sent to a site when e-mail for that site is received
-record(email_received, {to, from, localpart, localtags, domain, reference, email, headers, decoded, raw}).

% E-mail received notification:
% {z_convert:to_atom(Notification), received, UserId, ResourceId, Received}
% The {Notification, UserId, ResourceId} comes from m_email_receive_recipient:get_by_recipient/2.

%% @doc Bounced e-mail notification.  The recipient is the e-mail that is bouncing. When the
%% the message_nr is unknown the it is set to 'undefined'. This can happen when it is a "late bounce".
%% (notify)
-record(email_bounced, {message_nr, recipient}).

%% @doc Notify that we could send an e-mail (there might be a bounce later...)  (first)
-record(email_sent, {message_nr, recipient}).

%% @doc Notify that we could NOT send an e-mail (there might be a bounce later...)  (first)
-record(email_failed, {message_nr, recipient}).


%% @doc Add a handler for receiving e-mail notifications (first)
%% Returns {ok, LocalFrom} the unique localpart of an e-mail address on this server.
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

%% @doc Signal that the hierarchy underneath a resource has been changed by mod_menu (notify)
-record(hierarchy_updated, {root_id, predicate}).

%% @doc Resource is read, opportunity to add computed fields
%%      Used in a foldr with the read properties as accumulator.
-record(rsc_get, {id}).

%% @doc Resource will be deleted. (notify)
%% This notification is part of the delete transaction, it's purpose is to clean up
%% associated data.
-record(rsc_delete, {id}).

%% @doc Foldr for an resource insert, modify the insertion properties.
-record(rsc_insert, {}).

%% @doc Foldr for an resource update, modify the insertion properties.
%% The props are the resource's props _before_ the update.
%% The folded value is {IsChanged, UpdateProps} for the update itself.
%% Set IsChanged to true if you modify the UpdateProps.
-record(rsc_update, {action, id, props}).

%% @doc Resource update done notification.
%% action is 'insert', 'update' or 'delete'
-record(rsc_update_done, {action, id, pre_is_a, post_is_a, pre_props, post_props}).


%% @doc Add custom pivot fields to a resource's search index (map)
%% Result is a list of {module, props} pairs.
%% This will update a table "pivot_<module>".
%% You must ensure that the table exists.
-record(custom_pivot, {id}).

% 'pivot_rsc_data' - foldl over the resource props to extend/remove data to be pivoted

%% @doc Pivot just before a m_rsc_update update. Used to pivot fields before the pivot itself.
%%      Foldr over all observers.
-record(pivot_update, {id, raw_props}).

%% @doc Foldr over a resource's pivot data, after 'pivot_rsc_data' fold.
%% Further filtering/extending of pivot data.
-record(pivot_get, {id}).

%% @doc Fetch the list of ids whose title and id will be added to the pivot data of a resource
%% Foldr over the object ids of a resource.
-record(pivot_related, {id}).

%% @doc Determine which ids will have their title indexed with the resource.
%% Foldr over the related ids of a resource.
-record(pivot_related_text_ids, {id}).

%% @doc Foldr to change or add pivot fields for the main pivot table.
%%      The rsc contains all rsc properties for this resource, including pivot properties.
-record(pivot_fields, {id, rsc}).

%% @doc Signal that a resource pivot has been done. (notify)
-record(rsc_pivot_done, {id, is_a=[]}).


%% @doc Check if an action is allowed (first).
%% Should return undefined, true or false.
%% action :: view|update|delete
-record(acl_is_allowed, {action, object}).

%% @doc Check if an action on a property is allowed (first).
%% Should return undefined, true or false.
-record(acl_is_allowed_prop, {action, object, prop}).

%% @doc Filter the properties of a resource update, this is done on the raw data
%% A foldr over the update property list.
-record(acl_rsc_update_check, {id}).

%% @doc Return the maximum visible_for value an user can see. Used for optimizing queries. (first)
-record(acl_can_see, {}).

%% @doc Log an user on, fill the acl fields of the context (first)
%% Either return an updated #context or undefined
-record(acl_logon, {id}).

%% @doc Log an user off, clean the acl fields of the context (first)
%% Either return an updated #context or undefined
-record(acl_logoff, {}).


% Handle the confirm of an user.
% 'auth_confirm' - foldl over the #context
% 'auth_confirm_done' - notifications at the end of the confirmation
% 'auth_logon' - foldl over the #context (after setting the user_id)
% 'auth_logon_done' - notification that the logon of an user is done
% 'auth_logoff' - foldl over the #context before logging off
% 'auth_logoff_done' - notification before logging off, but after 'auth_logoff' and resetting the session user id
% 'auth_autologon' - (first) check if there is an automatic log on enabled for this session/user-agent
%                    returns {ok, UserId} when an user should be logged on.
%                    Called for every single request!
% 'session_context' - Initialize a context from the current session (foldl).
%                     Called on every request.
% 'session_init'    - Notification that a new session has been initialized (session_pid is in the context)
% 'session_init_fold' - foldl over the context containing a new session (after session_init)

%% @doc Check if an user is enabled (first)
%% Return true, false or undefined
-record(user_is_enabled, {id}).



%% @doc Fetch the url of a resource's html representation (first)
%% Returns {ok, Url} or undefined
-record(page_url, {id, is_a}).


%% @doc An edge has been inserted. (notify)
%% The predicate is an atom.
-record(edge_insert, {subject_id, predicate, object_id}).

%% @doc An edge has been deleted. (notify)
%% The predicate is an atom.
-record(edge_delete, {subject_id, predicate, object_id}).


%% @doc Notification that a site configuration is changed (notify)
-record(m_config_update, {module, key, value}).

%% @doc Notification that a site configuration's property is changed (notify)
-record(m_config_update_prop, {module, key, prop, value}).

%% @doc Notification that a medium file has been changed (notify)
%% The id is the resource id, medium contains the medium's property list.
-record(media_replace_file, {id, medium}).


%% @doc Send a notification that the resource 'id' is added to the query query_id. (notify)
-record(rsc_query_item, {query_id, match_id}).


%% @doc Add extra javascript with the {% script %} tag. (map)
%% Used to let modules inject extra javascript depending on the arguments of the {% script %} tag.
%% Must return an iolist()
-record(scomp_script_render, {is_nostartup=false, args=[]}).


%% @doc Find an import definition for a CSV file by checking the filename of the to be imported file. (first)
%% Should return the #import_csv_definition or undefined (in which case the column headers are used as property names).
-record(import_csv_definition, {basename, filename}).


%% @doc Handle an uploaded file which is part of a multiple file upload from an user-agent. (first)
%% Return a #context with the result or undefined when not handled.
%% The upload is a #upload record or a filename on the server.
-record(multiupload, {upload, query_args=[]}).

%% @doc Handle a new file received in the 'files/dropbox' folder of a site. (first)
% Unhandled files are deleted after a hour.
-record(dropbox_file, {filename}).

%% @doc Try to identify a file, returning a list of file properties. (first)
-record(media_identify_file, {filename}).

%% @doc Request to generate a HTML media viewer for a resource (first)
% Return {ok, Html} or undefined
-record(media_viewer, {id, props, filename, options=[]}).

%% @doc See if there is a 'still' image preview of a media item. (eg posterframe of a movie)
%% Return {ok, ResourceId} or undefined
-record(media_stillimage, {id, props=[]}).


%% @doc A survey has been filled in and submitted. (first)
-record(survey_submit, {id, answers, missing}).

%% @doc Check if the current user is allowed to download a survey. (first)
-record(survey_is_allowed_results_download, {id}).

%% @doc Put a value into the typed key/value store
-record(tkvstore_put, {type, key, value}).

%% @doc Get a value from the typed key/value store
-record(tkvstore_get, {type, key}).

%% @doc Delete a value from the typed key/value store
-record(tkvstore_delete, {type, key}).


%% @doc Broadcast notification.
-record(broadcast, {title=[], message=[], is_html=false, stay=true, type="error"}).

%% @doc Internal message of mod_development. Start a stream with debug information to the user agent.
%% 'target' is the id of the HTML element where the information is inserted.  
%% 'what' is the kind of debug information being streamed to the user-agent.
-record(debug_stream, {target, what=template}).

%% @doc Push some information to the debug page in the user-agent. 
% Will be displayed with io_lib:format("~p: ~p~n", [What, Arg]), be careful with escaping information!
-record(debug, {what, arg=[]}).


% Simple mod_development notifications:
% development_reload - Reload all template, modules etc
% development_make - Perform a 'make' on Zotonic, reload all new beam files




