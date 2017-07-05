%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2016 Marc Worrell
%% @doc Main definitions for zotonic

%% Copyright 2009-2016 Marc Worrell
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

%% @doc The request context, session information and other
-record(context, {
        %% Cowboy request data (only set when this context is used because of a request)
        req = undefined :: cowboy_req:req() | undefined,

        %% The site
        site = default :: atom(),

        %% The controller responsible for handling this request
        controller_module = undefined :: atom() | undefined,

        %% The page and session processes associated with the current request
        session_pid = undefined :: pid() | undefined,  % one session per browser (also manages the persistent data)
        session_id = undefined  :: binary() | undefined,
        page_pid = undefined    :: pid() | undefined,     % multiple pages per session, used for pushing information to the browser
        page_id = undefined     :: binary() | undefined,

        %% Servers and supervisors for the site
        depcache            :: pid() | atom(),
        notifier            :: pid() | atom(),
        session_manager     :: pid() | atom(),
        dispatcher          :: pid() | atom(),
        template_server     :: pid() | atom(),
        scomp_server        :: pid() | atom(),
        dropbox_server      :: pid() | atom(),
        pivot_server        :: pid() | atom(),
        module_indexer      :: pid() | atom(),
        translation_table   :: pid() | atom(),

        %% The database connection used for (nested) transactions, see z_db
        dbc = undefined     :: pid() | undefined,

        %% The pid of the database pool of this site and the db driver in use (usually z_db_pgsql)
        db = undefined      :: {atom(), atom()} | undefined,

        %% The language selected, used by z_trans and others
        %% The first language in the list is the selected language, the tail are the fallback languages
        language = [en]     :: [atom()],

        %% The timezone for this request
        tz = <<"UTC">>      :: binary(),

        %% The current logged on person, derived from the session and visitor
        acl = undefined     :: term() | undefined,      %% opaque placeholder managed by the z_acl module
        user_id = undefined :: integer() | undefined,

        %% The state below is the render state, can be cached and/or merged

        %% State of the current rendered template/scomp/page
        updates = []         :: list(),
        actions = []         :: list(),
        content_scripts = [] :: list(),
        scripts = []         :: list(),
        wire = []            :: list(),
        validators = []      :: list(),

        %% nested list with the accumulated html, xml or whatever output (mixed values)
        render = []          :: list(),

        %% Property list with context specific metadata
        props = []           :: proplists:proplist()
    }).


-define(SITE(Context), Context#context.site).
-define(DBC(Context), Context#context.dbc).

%% A date in the far future which will never happen.
%% This date is used as the "no end date" value.
-define(ST_JUTTEMIS, {{9999,8,17}, {12,0,0}}).

%% Record used for parsing multipart body (see z_parse_multipart)
-record(multipart_form, {name, data, filename, tmpfile, content_type, content_length, file, files=[], args=[]}).
-record(upload, {filename, tmpfile, data, mime}).

%% Record used for transporting data between the user-agent and the server.
-record(z_msg_v1, {
        qos = 0 :: 0 | 1 | 2,
        dup = false :: boolean(),
        msg_id :: binary(),
        timestamp :: pos_integer(),
        content_type = ubf :: text | javascript | json | form | ubf | atom() | binary(),
        delegate = postback :: postback | mqtt | atom() | binary(),
        push_queue = page :: page | session | user,

        % Set by transports from user-agent to server
        session_id :: binary(),
        page_id :: binary(),

        % Payload data
        data :: any()
    }).

-record(z_msg_ack, {
        qos = 1 :: 1 | 2,
        msg_id :: binary(),
        push_queue = page :: page | session | user,
        session_id :: binary(),
        page_id :: binary(),
        result :: any()
    }).

%% Model value interface for templates
-record(m, {model, value}).

%% Used for specifying resource id lists, as returned by object/subject lookup
-record(rsc_list, {list}).

%% Default page length for search
-define(SEARCH_PAGELEN, 20).

%% @doc A set of search results
-record(search_result, {
    result = [] :: list(),
    page = 1 :: pos_integer(),
    pagelen :: pos_integer(),
    total :: non_neg_integer(),
    all :: non_neg_integer(),
    pages :: non_neg_integer(),
    next,
    prev,
    facets = [] :: list()
}).

-record(m_search_result, {search_name, search_props, result, page, pagelen, total, pages, next, prev}).
-record(search_sql, {select, from, where="", order="", group_by="", limit, tables=[], args=[],
                     cats=[], cats_exclude=[], cats_exact=[], run_func, extra=[], assoc=false}).

%% Used for fetching the site dispatch rules (see also )
-record(site_dispatch_list, {
            site                       :: atom(),
            hostname = <<"localhost">> :: z_sites_dispatcher:hostname(),
            smtphost = undefined       :: z_sites_dispatcher:hostname() | undefined,
            hostalias = []             :: list(z_sites_dispatcher:hostname()),
            redirect = false           :: boolean(),
            dispatch_list = []         :: list(z_sites_dispatcher:dispatch_rule())
        }).

%% For z_supervisor, process definitions.
-record(child_spec, {name, mfa, status, pid, crashes=5, period=60,
                     period_retry=600, period_retries=10, eternal_retry=7200,
                     shutdown=5000}).


%% Used for storing templates/scomps etc. in the lookup ets table
-record(module_index_key, {
        site :: atom(),
        type :: z_module_indexer:key_type(),
        name :: binary()
    }).
-record(module_index, {
        key           :: #module_index_key{},
        filepath      :: filename:filename(),
        module        :: atom() | undefined,
        erlang_module :: atom() | undefined,
        tag           :: integer()
    }).

%% Name of the global module index table
-define(MODULE_INDEX, 'zotonic$module_index').

%% Index record for the mediaclass ets table.
-record(mediaclass_index_key, {site, mediaclass}).
-record(mediaclass_index, {key, props=[], checksum, tag}).

%% Name of the global mediaclass index table
-define(MEDIACLASS_INDEX, 'zotonic$mediaclass_index').

%% For the z_db definitions
-record(column_def, {name, type, length, is_nullable=true, default, primary_key, unique=false}).

%% For the datamodel: default resources to create.
-record(datamodel, {categories=[], predicates=[], resources=[], media=[], edges=[], data=[]}).

%% ACL administrator user id
-define(ACL_ADMIN_USER_ID, 1).
-define(ACL_ANY_USER_ID, -1).

%% ACL objects
-record(acl_rsc, {category, mime, size, props}).
-record(acl_edge, {
    subject_id :: m_rsc:resource(),
    predicate :: pos_integer() | atom(),
    object_id :: m_rsc:resource()
}).
-record(acl_media, {mime, size}).

%% ACL notifications
-record(acl_add_sql_check, {alias, args, search_sql}).


%% ACL fields for an acl check. Fields are initialized for the visible resource.
%% This is used for fetching the acl fields from a resource record.
-record(acl_props, {
    is_published = true :: boolean(),
    is_authoritative = true :: boolean(),
    publication_start ={{1900,1,1},{0,0,0}} :: calendar:datetime(),
    publication_end = ?ST_JUTTEMIS, %% :: calendar::datetime(),
    content_group_id = undefined :: undefined | m_rsc:resource_id(),
    visible_for = 0 :: non_neg_integer()
}).

%% @doc drag and drop event message
-record(dragdrop, {tag, delegate, id}).

%% @doc Template definition for z_render:update/insert (and others)
-record(render, {
    template :: string(),
    is_all = false :: boolean(),
    vars = [] :: proplists:proplist()
}).

%% @doc Data import definition. See also mod_import_csv.
-record(import_data_def, {colsep=$\t, skip_first_row=true, columns=[], importdef}).

%% @doc Check if an assumption is true
-define(ASSERT(A,E), z_utils:assert(A,E)).

%% @doc Call the translate function, 2nd parameter is context
-define(__(T,Context), z_trans:trans(T,Context)).

%% Number of seconds between two comet polls before the page expires
-define(SESSION_PAGE_TIMEOUT, 30).

%% Number of seconds between session expiration checks
-define(SESSION_CHECK_EXPIRE, 10).

%% Default session expiration in seconds.
%% The first keepalive message must be received before SESSION_EXPIRE_1 seconds
%% Subsequent messages must be received before SESSION_EXPIRE_N
-define(SESSION_EXPIRE_1,   40).
-define(SESSION_EXPIRE_N, 3600).

%% The name of the persistent data cookie
-define(PERSIST_COOKIE, <<"z_pid">>).

%% Max age of the person cookie, 10 years or so.
-define(PERSIST_COOKIE_MAX_AGE, 3600*24*3650).

%% Millisecs of no activity before the visitor process is stopped (if there are no attached sessions).
-define(VISITOR_TIMEOUT, 60 * 1000).

%% Some standard periods in seconds
-define(MINUTE,     60).
-define(HOUR,     3600).
-define(DAY,     86400).
-define(WEEK,   604800).
-define(YEAR, 31557600).

%% Our default WWW-Authenticate header
-define(WWW_AUTHENTICATE, <<"OAuth-1.0">>).

%% Notifier defines
-define(NOTIFIER_DEFAULT_PRIORITY, 500).

%% Wrapper macro to put Erlang terms in a bytea database column.
%% Extraction is automatic, based on a magic marker prefixed to the serialized term.
-define(DB_PROPS(N), {term, N}).

-include("zotonic_notifications.hrl").
-include("zotonic_events.hrl").
-include("zotonic_log.hrl").

