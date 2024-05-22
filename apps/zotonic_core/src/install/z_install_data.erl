%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell, Arjan Scherpenisse
%% @doc Initialize the database with start data.
%% @todo Insert these after translation functions have started so that we can use the
%% Zotonic .po files for selecting all translations of the predicates.
%% @end

%% Copyright 2009-2023 Marc Worrell, Arjan Scherpenisse
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
-spec install(Site, Context) -> ok when
    Site :: atom(),
    Context :: z:context().
install(Site, Context) ->
    ?LOG_NOTICE(#{
        text => <<"Site install start.">>,
        in => zotonic_core,
        site => Site
    }),
    ok = install_category(Context),
    ok = install_rsc(Context),
    ok = install_identity(Context),
    ok = install_predicate(Context),
    z_db:equery("SELECT setval('rsc_id_seq', m) FROM (select 1 + max(id) as m from rsc) sub", Context),
    ?LOG_NOTICE(#{
        text => <<"Site install done.">>,
        in => zotonic_core,
        site => Site
    }),
    ok.

install_category(C) ->
    ?LOG_INFO(#{
        text => <<"Site install: Inserting categories">>,
        in => zotonic_core
    }),
    %% The egg has to lay a fk-checked chicken here, so the insertion order is sensitive.

    %% 1. Insert the categories "meta" and "category"
    {ok, 2} = z_db:equery("
                    insert into hierarchy (name, id, parent_id, nr, lvl, lft, rght)
                    values
                        ('$category', 115, null, 90000000, 1, 90000000, 92000000),
                        ('$category', 116, null, 91000000, 2, 91000000, 91000000)
                    ", C),

    %% make "category" a sub-category of "meta"
    {ok, 1} = z_db:equery("update hierarchy set parent_id = 115 where id = 116", C),

    %% "http://purl.org/dc/terms/DCMIType" ?
    {ok, 1} = z_db:equery("
            insert into rsc (id, is_protected, visible_for, category_id, name, uri, props, language, publication_start)
            values (116, true, 0, 116, 'category', $1, $2, '{nl,en}', now())
            ", [    undefined,
                    ?DB_PROPS([{title, {trans, [{en, <<"Category">>}, {nl, <<"Categorie">>}]}}])
                ], C),

    {ok, 1} = z_db:equery("
            insert into rsc (id, is_protected, visible_for, category_id, name, uri, props, language, publication_start)
            values (115, true, 0, 116, 'meta', $1, $2, '{nl,en}', now())
            ", [    undefined,
                    ?DB_PROPS([{title, {trans, [{en, <<"Meta">>}, {nl, <<"Meta">>}]}}])
                ], C),

    %% Now that we have the category "category" we can insert all other categories.
    Cats = [
        {101,undefined,  1,1,1,1, other,       true,  undefined,                                   [{title, {trans, [{en, <<"Uncategorized">>}, {nl, <<"Zonder categorie">>}]}}] },

        {104,undefined,  2,1,2,4, text,        false, "http://purl.org/dc/dcmitype/Text",          [{title, {trans, [{en, <<"Text">>}, {nl, <<"Tekst">>}]}}] },
            {106,104,    3,2,3,3, article,     false, undefined,                                   [{title, {trans, [{en, <<"Article">>}, {nl, <<"Artikel">>}]}}] },
                {109,106,4,3,4,4, news,        false, undefined,                                   [{title, {trans, [{en, <<"News">>}, {nl, <<"Nieuws">>}]}}] },

        {102,undefined,  5,1,5,5, person,      true,  undefined,                                   [{title, {trans, [{en, <<"Person">>}, {nl, <<"Persoon">>}]}}] },

        {119,undefined,  6,1,6,7, location,    false, undefined,                                   [{title, {trans, [{en, <<"Location">>}, {nl, <<"Locatie">>}]}}] },
            {107,119,    7,2,7,7, website,     false, undefined,                                   [{title, {trans, [{en, <<"Website">>}, {nl, <<"Website">>}]}}] },

        {108, undefined, 8,1,8,8, event,       false, "http://purl.org/dc/dcmitype/Event",         [{title, {trans, [{en, <<"Event">>}, {nl, <<"Evenement">>}]}}] },

        {103,undefined,  9,1,9,9, artifact,    false, "http://purl.org/dc/dcmitype/PhysicalObject",[{title, {trans, [{en, <<"Artifact">>}, {nl, <<"Artefact">>}]}}] },

        {110,undefined,  10,1,10,14, media,       true,  "http://purl.org/dc/dcmitype/Image",         [{title, {trans, [{en, <<"Media">>}, {nl, <<"Media">>}]}}] },
            {111,110,    11,2,11,11, image,       true,  "http://purl.org/dc/dcmitype/StillImage",    [{title, {trans, [{en, <<"Image">>}, {nl, <<"Afbeelding">>}]}}] },
            {112,110,    12,2,12,12, video,       true,  "http://purl.org/dc/dcmitype/MovingImage",   [{title, {trans, [{en, <<"Video">>}, {nl, <<"Video">>}]}}] },
            {113,110,    13,2,13,13, audio,       true,  "http://purl.org/dc/dcmitype/Sound",         [{title, {trans, [{en, <<"Audio">>}, {nl, <<"Geluid">>}]}}] },
            {114,110,    14,2,14,14, document,    true,  undefined,							         [{title, {trans, [{en, <<"Document">>}, {nl, <<"Document">>}]}}] },

        {120,undefined,  15,1,15,16, collection,  false, "http://purl.org/dc/dcmitype/Collection",    [{title, {trans, [{en, <<"Collection">>}, {nl, <<"Collectie">>}]}}] },
            {121,120,    16,2,16,16, 'query',     false, "http://purl.org/dc/dcmitype/Dataset",       [{title, {trans, [{en, <<"Search query">>}, {nl, <<"Zoekopdracht">>}]}}] },

        {122,undefined,  17,1,17,18, categorization,true,undefined,                                   [{title, {trans, [{en, <<"Categorization">>}, {nl, <<"Categorisatie">>}]}}] },
            {123,122,    18,2,18,18, keyword,     true,  undefined,                                   [{title, {trans, [{en, <<"Keyword">>}, {nl, <<"Trefwoord">>}]}}] },

        % 115. Meta (see above)
            % 116. Category (see above)
            {117,115,    92,2,92,92, predicate,   true,  undefined,                                   [{title, {trans, [{en, <<"Predicate">>},     {nl, <<"Predikaat">>}]}}] }

        % Next id: 124
    ],

    InsertCatFun = fun({Id, ParentId, Nr, Lvl, Left, Right, Name, Protected, Uri, Props}) ->
        {ok, 1} = z_db:equery("
                insert into rsc (id, visible_for, category_id, is_protected, name, uri, props, language, publication_start)
                values ($1, 0, 116, $2, $3, $4, $5, '{nl,en}', now())
                ", [ Id, Protected, Name, Uri, ?DB_PROPS(Props) ], C),
        {ok, 1} = z_db:equery("
                insert into hierarchy (name, id, parent_id, nr, lvl, lft, rght)
                values ('$category', $1, $2, $3, $4, $5, $6)",
                [Id, ParentId, Nr*1000000, Lvl, Left*1000000, Right*1000000-1], C)
    end,
    lists:foreach(InsertCatFun, Cats),
    ok.


%% @doc Install some initial resources, most important is the system administrator
install_rsc(C) ->
    ?LOG_INFO(#{
        text => <<"Site install: inserting base resources (admin, etc.)">>,
        in => zotonic_core
    }),
    Rsc = [
        % id  vsfr  cat   protect name,         props
        [   1,  0,  102,  true,    "administrator",   ?DB_PROPS([{title,<<"Site Administrator">>}]) ]
    ],
    [ {ok,1} = z_db:equery("
            insert into rsc (id, visible_for, category_id, is_protected, name, props, language, publication_start)
            values ($1, $2, $3, $4, $5, $6, '{nl,en}', now())
            ", R, C) || R <- Rsc ],
    {ok, _} = z_db:equery("update rsc set creator_id = 1, modifier_id = 1, is_published = true", C),
    ok.


%% @doc Install the admin user as a user.  Uses the hard coded password "admin" when no password defined in the environment.
install_identity(C) ->
    ?LOG_INFO(#{
        text => <<"Site install: inserting username for the admin">>,
        in => zotonic_core
    }),
    Hash = m_identity:hash(<<>>),
    {ok, 1} = z_db:equery("
        insert into identity (rsc_id, type, key, is_unique, propb)
        values (1, 'username_pw', 'admin', true, $1)", [{term, Hash}], C),
    ok.


%% @doc Install some initial predicates, this list should be extended with common and useful predicates
%% See http://dublincore.org/documents/dcmi-terms/
install_predicate(C) ->
    ?LOG_INFO(#{
        text => <<"Site install: inserting predicates">>,
        in => zotonic_core
    }),
    Preds = [
        % id   protect name       uri                                                  props
        [ 300, true,   "about",    "http://www.w3.org/1999/02/22-rdf-syntax-ns#about",  ?DB_PROPS([{reversed, false},{title, #trans{ tr = [{en,<<"About">>},    {nl,<<"Over">>}]}}])],
        [ 301, true,   "author",   "http://purl.org/dc/terms/creator",                  ?DB_PROPS([{reversed, false},{title, #trans{ tr = [{en,<<"Author">>},   {nl,<<"Auteur">>}]}}])],
        [ 303, true,   "relation", "http://purl.org/dc/terms/relation",                 ?DB_PROPS([{reversed, false},{title, #trans{ tr = [{en,<<"Relation">>}, {nl,<<"Relatie">>}]}}])],
        [ 304, true,   "depiction","http://xmlns.com/foaf/0.1/depiction",               ?DB_PROPS([{reversed, false},{title, #trans{ tr = [{en,<<"Depiction">>},{nl,<<"Afbeelding">>}]}}])],
        [ 308, true,   "subject",  "http://purl.org/dc/elements/1.1/subject",           ?DB_PROPS([{reversed, false},{title, #trans{ tr = [{en,<<"Keyword">>},  {nl,<<"Trefwoord">>}]}}, {is_connect_checkbox, true}])],
        [ 309, true,   "hasdocument", "http://zotonic.net/predicate/hasDocument",       ?DB_PROPS([{reversed, false},{title, #trans{ tr = [{en,<<"Document">>}, {nl,<<"Document">>}]}}])],
		[ 310, true,   "haspart",  "http://purl.org/dc/terms/hasPart",					?DB_PROPS([{reversed, false},{title, #trans{ tr = [{en,<<"Contains">>}, {nl,<<"Bevat">>}]}}])]
    ],

    CatId   = z_db:q1("select id from rsc where name = 'predicate'", C),

    [ {ok,1} = z_db:equery("
            insert into rsc (id, visible_for, is_protected, name, uri, props, category_id, is_published, creator_id, modifier_id, language, publication_start)
            values ($1, 0, $2, $3, $4, $5, $6, true, 1, 1, '{nl,en}', now())
            ", R ++ [CatId], C) || R <- Preds],

    ObjSubj = [
        [300, true,  104], %  text   -> about     -> _
        [301, false, 102], %  _      -> author    -> person
        [304, false, 110], %  _      -> depiction -> image

        [308, true,  104], %  text     -> subject   -> _
        [308, true,  102], %  person   -> subject   -> _
        [308, true,  119], %  location -> subject   -> _
        [308, true,  108], %  event    -> subject   -> _
        [308, true,  103], %  artifact -> subject   -> _
        [308, true,  110], %  media    -> subject   -> _
        [308, true,  114], %  collection -> subject   -> _
        [308, false, 123], %  _      -> subject   -> keyword

        [309, true,  102], %  person   -> document -> _
        [309, true,  103], %  artifact -> document -> _
        [309, true,  104], %  text     -> document -> _
        [309, true,  119], %  location -> document -> _
        [309, false, 114], %  _        -> document -> media

        [310, true,  120]  %  collection -> haspart -> _
    ],

    [ {ok, 1} = z_db:equery("
            insert into predicate_category (predicate_id, is_subject, category_id)
            values ($1, $2, $3)", OS, C) || OS <- ObjSubj ],
    ok.
