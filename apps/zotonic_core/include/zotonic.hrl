%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2019 Marc Worrell
%% @doc Main definitions for zotonic

%% Copyright 2009-2019 Marc Worrell
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

%% @doc The request context, session information and other
-record(context, {
        %% Cowboy request data (only set when this context is used because of a request)
        %% Cowmachine requires this to be the first record field.
        cowreq = undefined :: cowboy_req:req() | undefined,
        cowenv = undefined :: cowboy_middleware:env() | undefined,

        %% Site
        site = default :: atom(),

        %% Controller responsible for handling this request (iff http request)
        controller_module = undefined :: atom() | undefined,

        %% The remote client performing this request
        client_id = undefined :: binary() | undefined,                      % MQTT client id
        client_topic = undefined :: mqtt_sessions:topic() | undefined,      % Topic where the client can be reached
        routing_id = undefined :: binary() | undefined,                     % Unique routing id

        %% User authenticated for this request
        acl = undefined          :: term() | admin | undefined,  %% opaque placeholder managed by the z_acl module
        acl_is_read_only = false :: boolean(),
        user_id = undefined      :: integer() | authenticated | undefined,

        %% Deprecated template render state, used for wires, actions and other embedded scripts.
        render_state = undefined :: undefined | z_render:render_state(),

        %% Database pool and the db driver (usually z_db_pgsql)
        db = undefined :: {atom(), atom()} | undefined,

        %% Database connection used for (nested) transactions, see z_db
        dbc = undefined :: pid() | undefined,

        %% Language, used by z_trans and others
        %% The first language in the list is the selected language, the tail are the fallback languages
        language = [en] :: [atom()],

        %% Timezone, defaults to UTC
        tz = <<"UTC">> :: binary(),

        %% Metadata, use z_context:set/3 and z_context:get/2
        %% Also stores the parsed query args (key 'q') and validated field inputs ('q_validated')
        props = #{} :: map(),

        %% Servers and supervisors for the site
        %% TODO: delete the following and replace with cached versions (smaller context)
        depcache            :: pid() | atom(),
        dispatcher          :: pid() | atom(),
        template_server     :: pid() | atom(),
        scomp_server        :: pid() | atom(),
        dropbox_server      :: pid() | atom(),
        pivot_server        :: pid() | atom(),
        module_indexer      :: pid() | atom(),
        translation_table   :: atom()
        %% End TODO
    }).

%% Wrapper macro to put Erlang terms in a bytea or jsonb database column.
%% Extraction is automatic, based on a magic marker prefixed to the serialized term.
-define(DB_PROPS(N), {term, N}).
-define(DB_PROPS_JSON(N), {term_json, N}).


%% A date in the far future which will never happen.
%% This date is used as the "no end date" value.
-define(ST_JUTTEMIS, {{9999,8,17}, {12,0,0}}).

%% Used for parsing multipart body (see z_parse_multipart)
-record(multipart_form, {
    name,
    data,
    filename,
    tmpfile,
    tmpmonitor,
    content_type,
    content_length,
    file,
    files = [],
    args = []
}).

%% Query argument value for uploaded files. Also used for email attachments.
-record(upload, {
    filename :: binary() | undefined,
    tmpfile :: file:filename_all() | undefined,
    tmpmonitor = undefined :: undefined | pid(),
    data = undefined :: binary() | undefined,
    mime = undefined :: binary() | undefined
}).


%% @doc e-mail notification used by z_email and z_email_server.
%% If the email is a received email then To/Cc/Bcc can be a list.
%% For sending only a single email address is supported.
-record(email, {
    to = [] :: list() | string() | binary() | m_rsc:resource_id() | undefined,
    cc = [] :: list() | string() | binary() | undefined,
    bcc = [] :: list() | string() | binary() | undefined,
    from = <<>> :: binary() | string(),
    reply_to = undefined :: binary() | string() | message_id | undefined,
    headers = [] :: [ {binary(), binary()} ],
    body,
    raw,
    subject :: iodata() | undefined,
    text :: iodata() | undefined,
    html :: iodata() | undefined,
    text_tpl :: template_compiler:template() | undefined,
    html_tpl :: template_compiler:template() | undefined,
    vars = [] :: list(),
    attachments = [] :: list(),
    queue = false :: boolean()
}).


%% Used for specifying resource id lists, as returned by object/subject lookup
-record(rsc_list, {
    list :: list( m_rsc:resource_id() )
}).

%% @doc Menu structures, a tree with a resource as a node.
-record(rsc_tree, {
    id = undefined :: m_rsc:resource() | undefined,
    tree = [] :: list( #rsc_tree{} )
}).

%% Default page length for search
-define(SEARCH_PAGELEN, 20).

%% @doc A set of search results. The atom/proplists name/args are deprecated, use
%% the binary / maps from now on.
-record(search_result, {
    search_name = <<"query">> :: binary() | atom(),
    search_args = #{} :: map() | proplists:proplist(),
    result = [] :: list(),
    page = 1 :: pos_integer(),
    pagelen = ?SEARCH_PAGELEN :: pos_integer() | undefined,
    options = #{} :: z_search:search_options(),
    total = undefined :: non_neg_integer() | undefined,
    pages = undefined :: non_neg_integer() | undefined,
    is_total_estimated = false :: boolean(),
    next = false :: pos_integer() | false,
    prev = 1 :: pos_integer(),
    facets = undefined :: #{ binary() => map() } | undefined
}).

-record(search_sql, {
    select :: iodata(),
    from :: iodata(),
    where = "" :: iodata(),
    order = "" :: iodata(),
    group_by = "" :: iodata(),
    limit,
    tables = [] :: list(),
    args = [] :: list(),
    cats = [] :: list(),            % Pairs {alias, cats}
    cats_exclude = [] :: list(),    % Pairs {alias, cats}
    cats_exact = [] :: list(),      % Pairs {alias, cats}
    run_func :: function() | undefined,
    post_func :: fun( (#search_result{}, #search_sql{}, z:context()) -> #search_result{} ) | undefined,
    extra = [] :: list(),
    assoc = false :: boolean(),
    search_sql_terms = undefined :: list() | undefined % Optional, if search was built using #search_sql_term{} records
}).

-record(search_sql_term, {
    label = undefined,
    select = [ <<"rsc.id">> ],
    tables = #{ <<"rsc">> => <<"rsc">> },
    join_inner = #{},
    join_left = #{},
    where = [],
    sort = [],
    asort = [],
    zsort = [],
    cats = [],
    cats_exclude = [],
    cats_exact = [],
    extra = [],
    args = []
}).

-record(search_sql_nested, {
    terms = [] :: [ #search_sql_term{} | #search_sql_nested{} ],
    operator = <<"allof">> :: binary()
}).

-record(search_sql_terms, {
    terms = [] :: [ #search_sql_term{} | #search_sql_nested{} ],
    post_func :: fun( (#search_result{}, #search_sql{}, z:context()) -> #search_result{} ) | undefined
}).


%% Used for fetching the site dispatch rules (see also )
-record(site_dispatch_list, {
    site                       :: atom(),
    hostname = <<"localhost">> :: z_sites_dispatcher:hostname(),
    smtphost = undefined       :: z_sites_dispatcher:hostname() | undefined,
    hostalias = []             :: list(z_sites_dispatcher:hostname()),
    redirect = false           :: boolean(),
    dispatch_list = []         :: list(z_sites_dispatcher:dispatch_rule()),
    page_paths = #{}           :: #{ atom() | binary() => z_sites_dispatcher:dispatch_rsc_rule() }
}).


%% Used for storing templates/scomps etc. in the lookup ets table
-record(module_index_key, {
    site :: atom(),
    type :: z_module_indexer:key_type() | undefined,
    name :: atom() | binary() | {binary(), binary()}
}).

-record(module_index, {
    key           :: #module_index_key{},
    filepath      :: file:filename_all() | undefined,
    module        :: atom() | undefined,
    erlang_module :: atom() | undefined,
    tag           :: integer() | undefined
}).

%% For the z_db definitions
-record(column_def, {
    name :: atom(),
    type :: string() | binary(),
    length = undefined :: non_neg_integer() | undefined,
    is_nullable = true :: boolean(),
    is_array = false :: boolean(),
    default = undefined,
    primary_key = false :: boolean(),
    unique = false :: boolean()
}).

%% For the datamodel: default resources to create.
-record(datamodel, {
    categories = [] :: list(),
    predicates = [] :: list(),
    resources = [] :: list(),
    media = [] :: list(),
    edges = [] :: list()
}).

%% Record could be returned by #dispatch notification
-record(dispatch_redirect, {
    location = <<>> :: binary(),
    is_permanent = false :: boolean()
}).

%% Record could be returned by #dispatch notification
-record(dispatch_match, {
    dispatch_name = undefined :: atom(),
    mod :: atom(),
    mod_opts = [] :: list(),
    path_tokens = [] :: list(binary()),
    bindings = [] :: list({atom(), binary() | true})
}).

%% Record could be returned by #dispatch notification
-record(dispatch_rules, {
    rules :: z_sites_dispatcher:site_dispatch_list() | undefined
}).


%% ACL administrator user id
-define(ACL_ADMIN_USER_ID, 1).
-define(ACL_ANY_USER_ID, -1).

%% ACL objects for the #acl_is_allowed{} notification0

%% Resource insert or update.
-record(acl_rsc, {
    id :: m_rsc:resource_id() | undefined,
    category :: atom(),
    props = #{} :: map()
}).

%% Edge insert and delete
-record(acl_edge, {
    subject_id :: m_rsc:resource(),
    predicate :: pos_integer() | atom(),
    object_id :: m_rsc:resource()
}).

%% Media uploads
-record(acl_media, {
    mime = <<"binary/octet-stream">> :: binary(),
    size = undefined :: undefined | non_neg_integer()
}).

%% MQTT acl check, called via the normal acl notifications.
%% Actions for these checks: subscribe, publish
-record(acl_mqtt, {
    topic :: list( binary() ),
    is_wildcard :: boolean(),
    packet :: mqtt_packet_map:mqtt_packet()
}).

%% ACL notifications
%% Modify queries by adding ACL restrictions by the database
-record(acl_add_sql_check, {
    alias,
    args,
    search_sql
}).


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


%% @doc Template definition for z_render:update/insert (and others)
-record(render, {
    template :: template_compiler:template(),
    is_all = false :: boolean(),
    vars = [] :: proplists:proplist()
}).

%% @doc Data import definition. See also mod_import_csv.
-record(import_data_def, {
    colsep = $\t :: 0..255,
    skip_first_row = true :: boolean(),
    columns = [] :: list(),
    importdef
}).


%% @doc Check if an assumption is true
-define(ASSERT(A,E), z_utils:assert(A,E)).

%% @doc Call the translate function for a string
-define(__(T,Context), z_trans:trans(T,Context)).

%% @doc Return all translations function for a string
-define(___(T,Context), z_trans:translations(T,Context)).

%% @doc Extra trans record definition to ease JSON mapping of translatable strings
-record(trans, { tr = [] :: list( {atom(), binary()} )}).

%% Some standard periods in seconds
-define(MINUTE,     60).
-define(HOUR,     3600).
-define(DAY,     86400).
-define(WEEK,   604800).
-define(YEAR, 31557600).

%% Our default WWW-Authenticate header
-define(WWW_AUTHENTICATE, <<"oauth2">>).

%% The default maxage of HSTS. Set to 200 days.
 -define(HSTS_MAXAGE, 3600*24*200).


-include("zotonic_notifications.hrl").
-include("zotonic_log.hrl").
-include("zotonic_wired.hrl").
-include("zotonic_deprecated.hrl").

