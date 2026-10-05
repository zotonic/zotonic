%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Compile language selection and install the versioned SQL translation helper.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_sparql_sql_translation).
-export([install/1, expression/4]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/z_sparql_sql.hrl").

%% @doc Install versioned helpers in the site schema. Future fixes get a new
%% function suffix and module schema version; keep old versions during upgrades.
-spec install(Context) -> ok when Context :: z:context().
install(Context) ->
    Priv = code:priv_dir(zotonic_mod_sparql),
    lists:foreach(fun(Name) ->
        {ok, Sql0} = file:read_file(filename:join([Priv, "sql", Name])),
        Sql = case Name of
            "language_chain_v2.sql" ->
                Json = z_json:encode(language_chains()),
                Escaped = binary:replace(Json, <<"'">>, <<"''">>, [global]),
                binary:replace(Sql0, <<"__LANGUAGE_CHAINS__">>, Escaped);
            _ -> Sql0
        end,
        [] = z_db:q(Sql, Context)
    end, ["translation_v1.sql", "translation_text_v1.sql", "language_chain_v2.sql",
        "translation_v2.sql", "translation_text_v2.sql"]),
    ok.

%% @doc Keep selection in SQL so projection, ordering and COALESCE share values.
-spec expression(Function, Value, Language, Context) -> #sql_expression{}
    when Function :: translation | translation_fallback,
         Value :: #sql_expression{}, Language :: #sql_expression{},
         Context :: z:context().
expression(Function, Value, Language, Context) ->
    Source0 = source(Value),
    Metadata = z_sparql_sql:expression_metadata(Value),
    SourceTag = maps:get(language, Metadata),
    % Already tagged literals (including nested lookups) remain language-specific.
    Source = ["CASE WHEN ", SourceTag, " IS NULL THEN ", Source0,
        " ELSE jsonb_build_object('_type', 'trans', 'tr', jsonb_build_object(",
        SourceTag, ", ", Source0, ")) END"],
    Lang = z_sparql_sql:coerce_expression(Language, text),
    Default = case Function of
        translation -> <<"NULL::text">>;
        translation_fallback ->
            Code = z_language:default_language(Context),
            % Language codes come from Zotonic's language registry.
            ["'", atom_to_binary(Code, utf8), "'::text"]
    end,
    Any = case Function of translation -> "false"; translation_fallback -> "true" end,
    Helper = case Function of
        translation -> "z_sparql_translation_v1";
        translation_fallback -> "z_sparql_translation_v2"
    end,
    Call = [Helper, "(", Source, ", (", Lang#sql_expression.sql,
        ")::text, ", Default, ", ", Any, ")"],
    Text = ["(", Call, " ->> 'value')"],
    Tag = ["(", Call, " ->> 'language')"],
    #sql_expression{
        sql = Text, type = text, source = expression,
        rdf = #{kind => <<"'literal'">>, language => Tag,
            datatype => ["CASE WHEN ", Tag, " IS NULL THEN '",
                "http://www.w3.org/2001/XMLSchema#string", "' ELSE '",
                "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString", "' END"]}
    }.

source(#sql_expression{source = jsonb, sql = Sql}) ->
    ["(", Sql, ")::jsonb"];
source(#sql_expression{type = Type, sql = Sql}) when Type =:= text; Type =:= any ->
    ["to_jsonb((", Sql, ")::text)"];
source(#sql_expression{type = Type}) ->
    throw({error, {invalid_translation_type, Type}}).

%% Use Zotonic's canonical aliases and explicit parent/script fallback rules.
%% Persist the registry with the helper version, also supporting row languages.
language_chains() ->
    maps:from_list([
        {Name, [atom_to_binary(L, utf8) || L <- [Code | z_language:fallback_language(Code)]]}
        || {Name, #{code_atom := Code}} <- maps:to_list(z_language_data:languages_map_flat()),
           is_binary(Name)
    ]).
