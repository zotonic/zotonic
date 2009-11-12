%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-07
%%
%% @doc Initialize the database with start data.

%% Copyright 2009 Marc Worrell
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

-module(z_install_data).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    install/1,
    install_category/1
]).

-include_lib("zotonic.hrl").

%% @doc Insert boot data into the database.
%% @spec install(Connection) -> ok
install(C) ->
    ok = install_config(C),
    ok = install_modules(C),
    ok = install_group(C),
    ok = install_category(C),
    ok = install_rsc(C),
    ok = install_identity(C),
    ok = install_predicate(C),
    ok = install_edge(C),
    ok = install_menu(C),
    ok.


%% @doc Install all configuration parameters with default values
%% @spec install_config(Connection) -> ok
install_config(C) ->
    ?DEBUG("Inserting config keys"),
    {ok, 1} = pgsql:equery(C, 
        "insert into config (module, key, value, props, modified) values ($1, $2, $3, $4, now())", 
        ["zotonic", "version", ?ZOTONIC_VERSION, []]),
    {ok, 1} = pgsql:equery(C, 
        "insert into config (module, key, value, props, modified) values ($1, $2, $3, $4, now())", 
        ["i18n", "language", "en", []]),
    pgsql:reset_id(C, "config"),
    ok.


install_modules(C) ->
    ?DEBUG("Inserting modules"),
    Modules = [
        "mod_test",
        "mod_base",
        "mod_emailer",
        "mod_menu",
        "mod_oauth",
        "mod_search",
        "mod_video_embed",
        "mod_atom_feed",
        
        "mod_seo",
        "mod_seo_google",
        "mod_seo_sitemap",

        "mod_admin",
        "mod_admin_address",
        "mod_admin_category",
        "mod_admin_config",
        "mod_admin_event",
        "mod_admin_group",
        "mod_admin_identity",
        "mod_admin_modules",
        "mod_admin_person",
        "mod_admin_predicate",

        % The default site
        "default"
    ],
    [
        {ok, 1} = pgsql:equery(C, "insert into module (name, is_active) values ($1, true)", [M]) || M <- Modules
    ],
    ok.


%% @doc Install the default admin, editor, supervisor, community and public groups
%% The resources will be inserted later, which is possible because the fk check is deferred till commit time.
install_group(C) ->
    ?DEBUG("Inserting groups"),
    Groups = [
        %   rsc admin  spvsr  cpub   ppub
        [204,  true,  true,  true,  true  ],
        [205,  false, false, true,  true  ],
        [206,  false, false, true,  false ],
        [207,  false, true,  false, false ],
        [208,  false, false, false, false ]
    ],
    
    [ {ok,1} = pgsql:equery(C, "
            insert into \"group\" (id, is_admin, is_supervisor, is_community_publisher, is_public_publisher) 
            values ($1, $2, $3, $4, $5)", R) || R <- Groups],
    ok.

install_category(C) ->
    ?DEBUG("Inserting categories"),
    %% The egg has to lay a fk-checked chicken here, so the insertion order is sensitive.

    %% 1. Insert the category "category" and "meta"
    {ok, 1} = pgsql:equery(C, "insert into category (id, parent_id, seq) values (116, null, 1)"),
    {ok, 1} = pgsql:equery(C, "insert into category (id, parent_id, seq) values (115, null, 99)"),
    {ok, 1} = pgsql:equery(C, "update category set parent_id = 115 where id = 116"),

    %% "http://purl.org/dc/terms/DCMIType" ?
    {ok, 1} = pgsql:equery(C, "
            insert into rsc (id, is_protected, visible_for, group_id, category_id, name, uri, props)
            values (116, true, 0, 204, 116, 'category', $1, $2)
            ", [    undefined, 
                    [{title, {trans, [{en, <<"Category">>}, {nl, <<"Categorie">>}]}}] 
                ]),

    {ok, 1} = pgsql:equery(C, "
            insert into rsc (id, is_protected, visible_for, group_id, category_id, name, uri, props)
            values (115, true, 0, 204, 116, 'meta', $1, $2)
            ", [    undefined, 
                    [{title, {trans, [{en, <<"Meta">>}, {nl, <<"Meta">>}]}}] 
                ]),
    
    %% Now that we have the category "category" we can insert all other categories.
    Cats = [
        % Meta categories for defining categories, predicates and groups.
            {117,115,    2, predicate,   true,  undefined,                                   [{title, {trans, [{en, <<"Predicate">>},     {nl, <<"Predikaat">>}]}}] },
            {118,115,    3, group,       true,  undefined,                                   [{title, {trans, [{en, <<"User Group">>},    {nl, <<"Gebruikersgroep">>}]}}] },

        %% Other categories
        {101,undefined,  1, other,       true,  undefined,                                   [{title, {trans, [{en, <<"Uncategorized">>}, {nl, <<"Zonder categorie">>}]}}] },

        {102,undefined,  3, person,      true,  undefined,                                   [{title, {trans, [{en, <<"Person">>}, {nl, <<"Persoon">>}]}}] },
            {121,102,    1, artist,      false, undefined,                                   [{title, {trans, [{en, <<"Artist">>}, {nl, <<"Artiest">>}]}}] },

        {104,undefined,  2, text,        false, "http://purl.org/dc/dcmitype/Text",          [{title, {trans, [{en, <<"Text">>}, {nl, <<"Tekst">>}]}}] },
            {106,104,    1, article,     false, undefined,                                   [{title, {trans, [{en, <<"Article">>}, {nl, <<"Artikel">>}]}}] },
                {109,106,1, news,        false, undefined,                                   [{title, {trans, [{en, <<"News">>}, {nl, <<"Nieuws">>}]}}] },
            {105,104,    2, review,      false, undefined,                                   [{title, {trans, [{en, <<"Review">>},  {nl, <<"Beoordeling">>}]}}] },

        {119,undefined,  4, location,    false, undefined,                                   [{title, {trans, [{en, <<"Location">>}, {nl, <<"Locatie">>}]}}] },
            {120,119,    1, venue,       false, undefined,                                   [{title, {trans, [{en, <<"Venue">>}, {nl, <<"Toneel">>}]}}] },

        {108, undefined, 5, event,       false, "http://purl.org/dc/dcmitype/Event",         [{title, {trans, [{en, <<"Event">>}, {nl, <<"Evenement">>}]}}] },

        {103,undefined,  6, artifact,    false, "http://purl.org/dc/dcmitype/PhysicalObject",[{title, {trans, [{en, <<"Artifact">>}, {nl, <<"Artefact">>}]}}] },
            {107,103,    1, product,     false, undefined,                                   [{title, {trans, [{en, <<"Product">>}, {nl, <<"Product">>}]}}] },

        {110,undefined,  7, media,       true,  "http://purl.org/dc/dcmitype/Image",         [{title, {trans, [{en, <<"Media">>}, {nl, <<"Media">>}]}}] }, 
            {111,110,    1, image,       true,  "http://purl.org/dc/dcmitype/StillImage",    [{title, {trans, [{en, <<"Image">>}, {nl, <<"Afbeelding">>}]}}] },
            {112,110,    2, video,       true,  "http://purl.org/dc/dcmitype/MovingImage",   [{title, {trans, [{en, <<"Video">>}, {nl, <<"Video">>}]}}] },
            {113,110,    3, sound,       true,  "http://purl.org/dc/dcmitype/Sound",         [{title, {trans, [{en, <<"Sound">>}, {nl, <<"Sound">>}]}}] },

        {114,undefined,  8, collection,  false, "http://purl.org/dc/dcmitype/Collection",    [{title, {trans, [{en, <<"Collection">>}, {nl, <<"Collectie">>}]}}] },

        {122,undefined,  9, categorization,true,undefined,                                   [{title, {trans, [{en, <<"Categorization">>}, {nl, <<"Categorisatie">>}]}}] },
            {123,122,    1, keyword,     true,  undefined,                                   [{title, {trans, [{en, <<"Keyword">>}, {nl, <<"Trefwoord">>}]}}] },
            {124,122,    2, genre,       true,  undefined,                                   [{title, {trans, [{en, <<"Genre">>}, {nl, <<"Genre">>}]}}] }

        % 115-118 meta -> @ position 99
        
        % Max: 124
    ],

    InsertCat = fun({Id, ParentId, Seq, Name, Protected, Uri, Props}) ->
        {ok, 1} = pgsql:equery(C, "
                insert into rsc (id, visible_for, group_id, category_id, is_protected, name, uri, props)
                values ($1, 0, 204, 116, $2, $3, $4, $5)
                ", [ Id, Protected, Name, Uri, Props ]),
        {ok, 1} = pgsql:equery(C, "
                insert into category (id, parent_id, seq)
                values ($1, $2, $3)", [Id, ParentId, Seq])
    end,
    [ InsertCat(R) || R <- Cats ],
    pgsql:reset_id(C, "rsc"),
    ok = enumerate_categories(C),
    ok.
    

%% @doc Install some initial resources, most important is the system administrator
%% @todo Add the hostname to the uri
install_rsc(C) ->
    ?DEBUG("Inserting base resources (group, admin, etc.)"),
    Rsc = [
        % id  vsfr  grp    cat   protect name,         props
        [ 204,  0,  204,   118,  true,   "admins",     [{title,<<"Administrators">>}] ],
        [ 205,  0,  205,   118,  false,  "editors",    [{title,<<"Site Editors">>}] ],
        [ 206,  0,  206,   118,  false,  "communityeditors", [{title,<<"Community Editors">>}] ],
        [ 207,  0,  207,   118,  false,  "supervisors", [{title,<<"Supervisors">>}] ],
        [ 208,  0,  208,   118,  false,  "content",    [{title,<<"Content">>}] ],

        [   1,  0,  204,   102,  true,    undefined,   [{title,<<"Site Administrator">>}] ],
        [ 500,  0,  204,   106,  false,   undefined,   [{title,<<"Welcome!">>}, {body, "<p>Hi! Welcome to your brand new Zotonic site.</p><p>Wanna change stuff? <a href=\"/admin\">Go to the Zotonic admin</a>.</p>"}] ],
        [ 501,  0,  204,   109,  false,   undefined,   [{title,<<"Some News">>}, {body, "<p>And the text of the news should be typed here.</p>"}] ]
    ],
    
    [ {ok,1} = pgsql:equery(C, "
            insert into rsc (id, visible_for, group_id, category_id, is_protected, name, props)
            values ($1, $2, $3, $4, $5, $6, $7)
            ", R) || R <- Rsc ],
    {ok, _} = pgsql:squery(C, "update rsc set creator_id = 1, modifier_id = 1, is_published = true"),

    % Page 500 is the home page by default
    {ok, _} = pgsql:squery(C, "update rsc set page_path='/', name='page_home' where id = 500"),
    
    pgsql:reset_id(C, "rsc"),

    % Connect person resources to the correct groups
    RscGroup = [
        % Id, Rsc  Grp    obsvr   leader
        [ 1,  1,   204,   false,  true ]
    ],

    [ {ok,1} = pgsql:equery(C, "
            insert into rsc_group (id, rsc_id, group_id, is_observer, is_leader)
            values ($1, $2, $3, $4, $5)
            ", R) || R <- RscGroup ],
    pgsql:reset_id(C, "rsc_group"),
    ok.


%% @doc Install the admin user as an user.  Uses the hard coded password "admin" when no password defined in the environment.
install_identity(C) ->
    ?DEBUG("Inserting username/password for the admin"),
    Password = case os:getenv("ZOTONIC_ADMINPASSWORD") of false -> "admin"; PW -> PW end,
    Hash = m_identity:hash(Password),
    {ok, 1} = pgsql:equery(C, "
        insert into identity (rsc_id, type, key, is_unique, propb)
        values (1, 'username_pw', 'admin', true, $1)", [Hash]),
    ok.
    

%% @doc Install some initial predicates, this list should be extended with common and useful predicates
%% @todo Extend and check this list.  Add allowed from/to categories.
%% @seealso http://dublincore.org/documents/dcmi-terms/
install_predicate(C) ->
    ?DEBUG("Inserting predicates"),
    Preds = [
        % id   protect name       uri                                                  props
        [ 300, true,   "about",    "http://www.w3.org/1999/02/22-rdf-syntax-ns#about",  [{reversed, false},{title, {trans, [{en,"About"},    {nl,"Over"}]}}]],
        [ 301, true,   "author",   "http://purl.org/dc/terms/creator",                  [{reversed, false},{title, {trans, [{en,"Author"},   {nl,"Auteur"}]}}]],
        [ 302, true,   "hasreview","http://purl.org/stuff/rev#hasReview",               [{reversed, false},{title, {trans, [{en,"Review"},   {nl,"Beoordeling"}]}}]],
        [ 303, true,   "relation", "http://purl.org/dc/terms/relation",                 [{reversed, false},{title, {trans, [{en,"Relation"}, {nl,"Relatie"}]}}]],
        [ 304, true,   "depiction","http://xmlns.com/foaf/0.1/depiction",               [{reversed, false},{title, {trans, [{en,"Depiction"},{nl,"Afbeelding"}]}}]],

        [ 305, true,   "atvenue",  "http://zotonic.net/predicate/atvenue",              [{reversed, false},{title, "Venue"}]],
        [ 306, true,   "performer","http://zotonic.net/predicate/performer",            [{reversed, false},{title, "Performer"}]],
        [ 307, true,   "hasgenre", "http://zotonic.net/predicate/hasgenre",             [{reversed, false},{title, "Genre"}]],

        [ 308, true,   "subject",  "http://purl.org/dc/elements/1.1/subject",           [{reversed, false},{title, {trans, [{en,"Keyword"},  {nl,"Trefwoord"}]}}]],

        [ 309, true,   "document", "http://zotonic.net/predicate/document",             [{reversed, false},{title, "Document"}]],
		[ 310, true,   "haspart",  "http://purl.org/dc/terms/hasPart",					[{reversed, false},{title, "Contains"}]]
    ],

    {ok, CatId}   = pgsql:squery1(C, "select id from rsc where name = 'predicate'"),
    {ok, GroupId} = pgsql:squery1(C, "select id from rsc where name = 'admins'"),
    
    [ {ok,1} = pgsql:equery(C, "
            insert into rsc (id, visible_for, is_protected, name, uri, props, group_id, category_id, is_published, creator_id, modifier_id)
            values ($1, 0, $2, $3, $4, $5, $6, $7, true, 1, 1)
            ", R ++ [GroupId,CatId]) || R <- Preds],
    pgsql:reset_id(C, "rsc"),

    ObjSubj = [
        {300, true,  104}, %  text   -> about     -> _
        {301, false, 102}, %  _      -> author    -> person
        {302, false, 105}, %  _      -> hasreview -> review
        {304, false, 110}, %  _      -> depiction -> image

        {305, true,  108}, %  Event  -> atvenue   -> _
        {305, false, 120}, %  _      -> atvenue   -> venue

        {306, true,  108}, %  Event  -> performer -> _
        {306, false, 121}, %  _      -> performer -> artist

        {307, true,  108}, %  Event  -> hasgenre  -> _
        {307, false, 124}, %  _      -> hasgenre  -> genre

        {308, true,  104}, %  text     -> subject   -> _
        {308, true,  102}, %  person   -> subject   -> _
        {308, true,  119}, %  location -> subject   -> _
        {308, true,  108}, %  event    -> subject   -> _
        {308, true,  103}, %  artifact -> subject   -> _
        {308, true,  110}, %  media    -> subject   -> _
        {308, true,  114}, %  collection -> subject   -> _
        {308, false, 123}, %  _      -> subject   -> keyword

        {309, true,  102}, %  person   -> document -> _
        {309, true,  103}, %  artifact -> document -> _
        {309, true,  104}, %  text     -> document -> _
        {309, true,  119}, %  location -> document -> _
        {309, false, 110}, %  _        -> document -> media

        {310, true,  114}  %  collection -> haspart -> _
    ],
    
    [ {ok, 1} = pgsql:equery(C, "
            insert into predicate_category (predicate_id, is_subject, category_id) 
            values ($1, $2, $3)", OS) || OS <- ObjSubj ],
    ok.


%% @doc Install example edges between the predefined content
install_edge(C) ->
    ?DEBUG("Inserting sample edge"),
    Edges = [
        %  subj  obj  pred    seq
        [  501,  500,   301,    1  ]
    ],
    
    [ {ok,1} = pgsql:equery(C, "
            insert into edge (subject_id, object_id, predicate_id, seq)
            values ($1, $2, $3, $4)
            ", R) || R <- Edges],
    pgsql:reset_id(C, "edge"),
    ok.


%% @doc Install the default menu.
install_menu(C) ->
    Menu = [{500,[]},{501,[]}],
    {ok, 1} = pgsql:equery(C, 
        "insert into config (module, key, value, props, modified) values ($1, $2, $3, $4, now())", 
        ["menu", "menu_default", "", [{menu, Menu}]]),
    ok.


%% @doc Enumerate all categories so that their left, right, level en nr are set correctly
%% @type enumerate_categories(Connection) -> ok
enumerate_categories(C) ->
    ?DEBUG("Sorting the category hierarchy"),
    {ok, _, CatTuples} = pgsql:equery(C, "select id, parent_id, seq from category order by seq, id"),
    Enums = m_category:enumerate(CatTuples),
    [
        {ok, _} = pgsql:equery(C, "update category set nr = $2, lvl = $3, lft = $4, rght = $5, props = $6 where id = $1", [CatId, Nr, Level, Left, Right, [{path,Path}]])
        || {CatId, Nr, Level, Left, Right, Path} <- Enums
    ],
    ok.
