%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2011 Marc Worrell
%% @doc Main definitions for zotonic

%% Copyright 2009-2011 Marc Worrell
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

%% The release information
-include("zotonic_release.hrl").

-include("zotonic_notifications.hrl").

-include_lib("deps/webmachine/include/wm_reqdata.hrl").

%% @doc The request context, session information and other
-record(context, {
        %% Webmachine request data
        wm_reqdata,
        
        %% The resource responsible for handling this request
        resource_module,
        
        %% The page (comet) and session processes associated with the current request
        session_pid=undefined,  % one session per browser (also manages the persistent data)
        page_pid=undefined,     % multiple pages per session
        page_id=undefined,

        %% The host (also the id of the database used) 
        host=default,
        
        %% Servers and supervisors for the site/host
        depcache,
        notifier,
        session_manager,
        dispatcher,
        template_server,
        scomp_server,
        dropbox_server,
        pivot_server,
        module_indexer,
        translation_table,
        
        %% The database connection used for (nested) transactions, see z_db
        dbc=undefined,

        %% The language selected, used by z_trans and others
        language=en,
        
        %% The current logged on person, derived from the session and visitor
        acl=undefined,      %% opaque placeholder managed by the z_acl module
        user_id=undefined,

        %% The state below is the render state, can be cached and/or merged
        
        %% State of the current rendered template/scomp/page
        updates=[],
        actions=[],
        content_scripts=[],
        scripts=[],
        wire=[],
        validators=[],

        %% iolist with the accumulated html, xml or whatever output
        render=[],

        %% Property list with context specific metadata
        props=[]
    }).
    
    
-define(WM_REQ(ReqData, Context), z_context:set_reqdata(ReqData, Context)).
-define(WM_REPLY(Reply, Context), {Reply, Context#context.wm_reqdata, Context#context{wm_reqdata=undefined}}).

-define(HOST(Context), Context#context.host).
-define(DBC(Context), Context#context.dbc).

-define(ST_JUTTEMIS, {{9999,8,17}, {12,0,0}}).

%% Record used for parsing multipart body (see z_parse_multipart)
-record(multipart_form, {name, data, filename, tmpfile, content_type, content_length, file, files=[], args=[]}).
-record(upload, {filename, tmpfile, data, mime}).

%% Model value interface for templates
-record(m, {model, value}).

%% Used for specifying resource id lists, as returned by object/subject lookup
-record(rsc_list, {list}).

%% Default page length for search
-define(SEARCH_PAGELEN, 20).

%% Used for search results
-record(search_result, {result=[], page=1, pagelen, total, all, pages, next, prev}).
-record(m_search_result, {search_name, search_props, result, page, pagelen, total, pages, next, prev}).
-record(search_sql, {select, from, where="", order="", group_by="", limit, tables=[], args=[], cats=[], cats_exclude=[], run_func, extra=[], assoc=false}).

%% For z_supervisor, process definitions.
-record(child_spec, {name, mfa, status, pid, crashes=5, period=60, 
                     period_retry=600, period_retries=10, eternal_retry=7200,
                     shutdown=5000}).

%% For the z_db definitions
-record(column_def, {name, type, length, is_nullable=true, default, primary_key}).

%% For the datamodel: default resources to create.
-record(datamodel, {categories=[], predicates=[], resources=[], media=[], edges=[]}).

%% ACL administrator user id
-define(ACL_ADMIN_USER_ID, 1).
-define(ACL_ANONYMOUS_USER_ID, -1).

%% ACL visibility levels
-define(ACL_VIS_USER, 3).
-define(ACL_VIS_GROUP, 2).
-define(ACL_VIS_COMMUNITY, 1).
-define(ACL_VIS_PUBLIC, 0).

%% ACL objects
-record(acl_rsc, {category, mime, size}).
-record(acl_edge, {subject_id, predicate, object_id}).
-record(acl_media, {mime, size}).

%% ACL notifications
-record(acl_add_sql_check, {alias, args, search_sql}).


%% ACL fields for an acl check. Fields are initialized for the visible resource.
%% This is used for fetching the acl fields from a resource record.
-record(acl_props, {
    is_published=true,
    is_authoritative=true,
    visible_for=?ACL_VIS_PUBLIC,
    publication_start={{1900,1,1},{0,0,0}},
    publication_end=?ST_JUTTEMIS
}).

%% @doc drag and drop event message
-record(dragdrop, {tag, delegate, id}).

%% @doc Template definition for z_render:update/insert (and others)
-record(render, {template, vars=[]}).

%% @doc Data import definition. See also mod_import_csv.
-record(import_data_def, {colsep=$\t, skip_first_row=true, record, importdef}).

%% @doc Check if an assumption is true
-define(ASSERT(A,E), z_utils:assert(A,E)).

%% @doc Call the translate function, 2nd parameter is context
-define(__(T,Context), z_trans:trans(T,Context)).

%% The name of the session request parameter
-define(SESSION_PAGE_Q, "z_pageid").

%% Number of seconds between two comet polls before the page expires
-define(SESSION_PAGE_TIMEOUT, 20).

%% Number of seconds between session expiration checks
-define(SESSION_CHECK_EXPIRE, 10).

%% Default session expiration in seconds.
%% The first keepalive message must be received before SESSION_EXPIRE_1 seconds
%% Subsequent messages must be received before SESSION_EXPIRE_N
-define(SESSION_EXPIRE_1,   40).
-define(SESSION_EXPIRE_N, 3600).

%% Millisecs of no activity before the visitor process is stopped (if there are no attached sessions).
-define(VISITOR_TIMEOUT, 60 * 1000).

%% Some standard periods in seconds
-define(MINUTE,     60).
-define(HOUR,     3600).
-define(DAY,     86400).
-define(WEEK,   604800).
-define(YEAR, 31557600).

%% Our default WWW-Authenticate header
-define(WWW_AUTHENTICATE, "OAuth-rsn").

%% Notifier defines
-define(NOTIFIER_DEFAULT_PRIORITY, 500).

%% Below is copied (and adapted) from Nitrogen, which is copyright 2008-2009 Rusty Klophaus

%%% LOGGING %%%
-define(DEBUG(Msg), z:debug_msg(?MODULE, ?LINE, Msg)).
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-define(LOG(Msg, Args), error_logger:info_msg(Msg, Args)).
-define(ERROR(Msg, Args), error_logger:error_msg("~p:~p "++Msg, [?MODULE, ?LINE|Args])).

-define(zDebug(Msg, Context), z:debug(Msg, [{module, ?MODULE}, {line, ?LINE}], Context)).
-define(zInfo(Msg, Context), z:info(Msg, [{module, ?MODULE}, {line, ?LINE}], Context)).
-define(zWarning(Msg, Context), z:warning(Msg, [{module, ?MODULE}, {line, ?LINE}], Context)).
