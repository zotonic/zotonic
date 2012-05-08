%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-07
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
    install/2,
    install_category/1
]).

-include_lib("zotonic.hrl").

%% @doc Insert boot data into the database.
%% @spec install(Host::atom(), Connection) -> ok
install(Host, C) ->
    ?DEBUG({Host, "Install start."}),
    ok = install_config(C),
    ok = install_modules(Host, C),
    ok = install_category(C),
    ok = install_rsc(C),
    ok = install_identity(C),
    ok = install_predicate(C),
    ?DEBUG({Host, "Install done."}),
    ok.

%% @doc Install all configuration parameters with default values
%% @spec install_config(Connection) -> ok
install_config(_C) ->
    ?DEBUG("Inserting config keys"),
    %% {ok, 1} = pgsql:equery(C, 
    %%     "insert into config (module, key, value, props, modified) values ($1, $2, $3, $4, now())", 
    %%     ["mod_foo", "bar", "baz", []]),
    %% pgsql:reset_id(C, "config"),
    ok.


install_modules(Host, C) ->
    ?DEBUG("Inserting modules"),
    Modules = [
        "mod_base",
        "mod_menu",
        "mod_oauth",
        "mod_search",
        "mod_oembed",
        "mod_atom_feed",
        "mod_translation",
        "mod_signal",
        "mod_logging",

        "mod_seo",
        "mod_seo_google",
        "mod_seo_sitemap",

        "mod_authentication",
        "mod_acl_adminonly",

        "mod_admin",
        "mod_admin_category",
        "mod_admin_config",
        "mod_admin_identity",
        "mod_admin_modules",
        "mod_admin_predicate",
        
        "mod_l10n",
        "mod_geomap",

        %% Enable comments
        "mod_comment",

        % The site-specific site
        atom_to_list(Host)
    ],
    [
        {ok, 1} = pgsql:equery(C, "insert into module (name, is_active) values ($1, true)", [M]) || M <- Modules
    ],
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
            insert into rsc (id, is_protected, visible_for, category_id, name, uri, props)
            values (116, true, 0, 116, 'category', $1, $2)
            ", [    undefined, 
                    [{title, {trans, [{en, <<"Category">>}, {nl, <<"Categorie">>}]}}] 
                ]),

    {ok, 1} = pgsql:equery(C, "
            insert into rsc (id, is_protected, visible_for, category_id, name, uri, props)
            values (115, true, 0, 116, 'meta', $1, $2)
            ", [    undefined, 
                    [{title, {trans, [{en, <<"Meta">>}, {nl, <<"Meta">>}]}}] 
                ]),
    
    %% Now that we have the category "category" we can insert all other categories.
    Cats = [
        % Meta categories for defining categories, predicates.
            {117,115,    2, predicate,   true,  undefined,                                   [{title, {trans, [{en, <<"Predicate">>},     {nl, <<"Predikaat">>}]}}] },

        %% Other categories
        {101,undefined,  1, other,       true,  undefined,                                   [{title, {trans, [{en, <<"Uncategorized">>}, {nl, <<"Zonder categorie">>}]}}] },

        {102,undefined,  3, person,      true,  undefined,                                   [{title, {trans, [{en, <<"Person">>}, {nl, <<"Persoon">>}]}}] },

        {104,undefined,  2, text,        false, "http://purl.org/dc/dcmitype/Text",          [{title, {trans, [{en, <<"Text">>}, {nl, <<"Tekst">>}]}}] },
            {106,104,    1, article,     false, undefined,                                   [{title, {trans, [{en, <<"Article">>}, {nl, <<"Artikel">>}]}}] },
                {109,106,1, news,        false, undefined,                                   [{title, {trans, [{en, <<"News">>}, {nl, <<"Nieuws">>}]}}] },

        {119,undefined,  4, location,    false, undefined,                                   [{title, {trans, [{en, <<"Location">>}, {nl, <<"Locatie">>}]}}] },
            {107,119,    4, website,     false, undefined,                                   [{title, {trans, [{en, <<"Website">>}, {nl, <<"Website">>}]}}] },

        {108, undefined, 5, event,       false, "http://purl.org/dc/dcmitype/Event",         [{title, {trans, [{en, <<"Event">>}, {nl, <<"Evenement">>}]}}] },

        {103,undefined,  6, artifact,    false, "http://purl.org/dc/dcmitype/PhysicalObject",[{title, {trans, [{en, <<"Artifact">>}, {nl, <<"Artefact">>}]}}] },

        {110,undefined,  7, media,       true,  "http://purl.org/dc/dcmitype/Image",         [{title, {trans, [{en, <<"Media">>}, {nl, <<"Media">>}]}}] }, 
            {111,110,    1, image,       true,  "http://purl.org/dc/dcmitype/StillImage",    [{title, {trans, [{en, <<"Image">>}, {nl, <<"Afbeelding">>}]}}] },
            {112,110,    2, video,       true,  "http://purl.org/dc/dcmitype/MovingImage",   [{title, {trans, [{en, <<"Video">>}, {nl, <<"Video">>}]}}] },
            {113,110,    3, audio,       true,  "http://purl.org/dc/dcmitype/Sound",         [{title, {trans, [{en, <<"Audio">>}, {nl, <<"Geluid">>}]}}] },
            {114,110,    4, document,    true,  undefined,							         [{title, {trans, [{en, <<"Document">>}, {nl, <<"Document">>}]}}] },

        {120,undefined,  8, collection,  false, "http://purl.org/dc/dcmitype/Collection",    [{title, {trans, [{en, <<"Collection">>}, {nl, <<"Collectie">>}]}}] },
            {121,120,    8, 'query',     false, "http://purl.org/dc/dcmitype/Dataset",       [{title, {trans, [{en, <<"Search query">>}, {nl, <<"Zoekopdracht">>}]}}] },

        {122,undefined,  9, categorization,true,undefined,                                   [{title, {trans, [{en, <<"Categorization">>}, {nl, <<"Categorisatie">>}]}}] },
            {123,122,    1, keyword,     true,  undefined,                                   [{title, {trans, [{en, <<"Keyword">>}, {nl, <<"Trefwoord">>}]}}] }

        % 115-118 meta -> @ position 99
        
        % Max: 124
    ],

    InsertCat = fun({Id, ParentId, Seq, Name, Protected, Uri, Props}) ->
        {ok, 1} = pgsql:equery(C, "
                insert into rsc (id, visible_for, category_id, is_protected, name, uri, props)
                values ($1, 0, 116, $2, $3, $4, $5)
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
    ?DEBUG("Inserting base resources (admin, etc.)"),
    Rsc = [
        % id  vsfr  cat   protect name,         props
        [   1,  0,  102,  true,    "administrator",   [{title,<<"Site Administrator">>}] ]
    ],
    
    [ {ok,1} = pgsql:equery(C, "
            insert into rsc (id, visible_for, category_id, is_protected, name, props)
            values ($1, $2, $3, $4, $5, $6)
            ", R) || R <- Rsc ],
    {ok, _} = pgsql:squery(C, "update rsc set creator_id = 1, modifier_id = 1, is_published = true"),
    pgsql:reset_id(C, "rsc"),
    ok.


%% @doc Install the admin user as an user.  Uses the hard coded password "admin" when no password defined in the environment.
install_identity(C) ->
    ?DEBUG("Inserting username for the admin"),
    Hash = m_identity:hash([]),
    {ok, 1} = pgsql:equery(C, "
        insert into identity (rsc_id, type, key, is_unique, propb)
        values (1, 'username_pw', 'admin', true, $1)", [Hash]),
    ok.
    

%% @doc Install some initial predicates, this list should be extended with common and useful predicates
%% See http://dublincore.org/documents/dcmi-terms/
%% @todo Extend and check this list.  Add allowed from/to categories.
install_predicate(C) ->
    ?DEBUG("Inserting predicates"),
    Preds = [
        % id   protect name       uri                                                  props
        [ 300, true,   "about",    "http://www.w3.org/1999/02/22-rdf-syntax-ns#about",  [{reversed, false},{title, {trans, [{en,"About"},    {nl,"Over"}]}}]],
        [ 301, true,   "author",   "http://purl.org/dc/terms/creator",                  [{reversed, false},{title, {trans, [{en,"Author"},   {nl,"Auteur"}]}}]],
        [ 303, true,   "relation", "http://purl.org/dc/terms/relation",                 [{reversed, false},{title, {trans, [{en,"Relation"}, {nl,"Relatie"}]}}]],
        [ 304, true,   "depiction","http://xmlns.com/foaf/0.1/depiction",               [{reversed, false},{title, {trans, [{en,"Depiction"},{nl,"Afbeelding"}]}}]],
        [ 308, true,   "subject",  "http://purl.org/dc/elements/1.1/subject",           [{reversed, false},{title, {trans, [{en,"Keyword"},  {nl,"Trefwoord"}]}}]],
        [ 309, true,   "hasdocument", "http://zotonic.net/predicate/hasDocument",       [{reversed, false},{title, {trans, [{en,"Document"}, {nl,"Document"}]}}]],
		[ 310, true,   "haspart",  "http://purl.org/dc/terms/hasPart",					[{reversed, false},{title, {trans, [{en,"Contains"}, {nl,"Bevat"}]}}]]
    ],

    {ok, CatId}   = pgsql:squery1(C, "select id from rsc where name = 'predicate'"),
    
    [ {ok,1} = pgsql:equery(C, "
            insert into rsc (id, visible_for, is_protected, name, uri, props, category_id, is_published, creator_id, modifier_id)
            values ($1, 0, $2, $3, $4, $5, $6, true, 1, 1)
            ", R ++ [CatId]) || R <- Preds],
    pgsql:reset_id(C, "rsc"),

    ObjSubj = [
        {300, true,  104}, %  text   -> about     -> _
        {301, false, 102}, %  _      -> author    -> person
        {304, false, 110}, %  _      -> depiction -> image

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
        {309, false, 114}, %  _        -> document -> media

        {310, true,  120}  %  collection -> haspart -> _
    ],
    
    [ {ok, 1} = pgsql:equery(C, "
            insert into predicate_category (predicate_id, is_subject, category_id) 
            values ($1, $2, $3)", OS) || OS <- ObjSubj ],
    ok.


%% @doc Enumerate all categories so that their left, right, level en nr are set correctly
%% @spec enumerate_categories(Connection) -> ok
enumerate_categories(C) ->
    ?DEBUG("Sorting the category hierarchy"),
    {ok, _, CatTuples} = pgsql:equery(C, "select id, parent_id, seq from category order by seq, id"),
    Enums = m_category:enumerate(CatTuples),
    [
        {ok, _} = pgsql:equery(C, "update category set nr = $2, lvl = $3, lft = $4, rght = $5, props = $6 where id = $1", [CatId, Nr, Level, Left, Right, [{path,Path}]])
        || {CatId, Nr, Level, Left, Right, Path} <- Enums
    ],
    ok.

