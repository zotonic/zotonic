%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2026 Marc Worrell
%% @doc Translate english sentences into other languages, following
%% the GNU gettext principle.
%% @end

%% Copyright 2009-2026 Marc Worrell
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

-module(z_trans).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    translations/2,
    translations/3,
    translations/4,
    trans_msgctxt/3,
    trans_msgctxt/4,
    plural_translations/3,
    plural_translations/4,
    plural_translations/5,
    trans_plural/4,
    trans_plural/5,
    parse_translations/1,
    trans/2,
    trans/3,
    lookup/2,
    lookup/3,
    lookup_fallback/2,
    lookup_fallback/3,
    lookup_fallback_languages/2,
    lookup_fallback_language/2,
    lookup_fallback_language/3
]).

-include_lib("../../include/zotonic.hrl").

%% @doc Fetch all translations for the given English string. If no translations are
%% found then a trans record with the given string is returned.
-spec translations(z:trans() | binary() | string(), z:context()) -> z:trans().
translations(#trans{ tr = Tr0 } = Trans0, Context) ->
    case lists:keyfind(en, 1, Tr0) of
        {en, From} ->
            #trans{ tr = Tr1 } = translations(From, Context),
            #trans{ tr = merge_trs(Tr0, lists:reverse(Tr1)) };
        false ->
            Trans0
    end;
translations(From, Context) when is_binary(From) ->
    try
        case ets:lookup(z_trans_server:table(Context), From) of
            [] ->
    			#trans{ tr = [ {en, From} ] };
            [{_, Trans}] ->
    			#trans{ tr = Trans }
        end
    catch
        error:badarg ->
            #trans{ tr = [ {en, From} ] }
    end;
translations(From, Context) when is_list(From) ->
    translations(unicode:characters_to_binary(From), Context);
translations(From, Context) ->
    translations(z_convert:to_binary(From), Context).

%% @doc Fetch a message in a gettext context, without contextless fallback.
%% Use lookup/lookup_fallback to select from the returned record; trans/3 can
%% re-fetch a record by its English text and does not retain catalog identity.
-spec translations(Text, MsgCtxt, Context) -> #trans{} when
    Text :: binary() | string(), MsgCtxt :: binary() | string(), Context :: z:context().
translations(Text, MsgCtxt, Context) ->
    translations(Text, MsgCtxt, false, Context).

%% @doc With Fallback=true, prefer contextual then contextless text per language.
-spec translations(Text, MsgCtxt, Fallback, Context) -> #trans{} when
    Text :: binary() | string(), MsgCtxt :: binary() | string(),
    Fallback :: boolean(), Context :: z:context().
translations(Text, MsgCtxt, Fallback, Context) when is_boolean(Fallback) ->
    Source = unicode:characters_to_binary(Text),
    Tr = contextual_translations(Source, unicode:characters_to_binary(MsgCtxt), Fallback, Context),
    #trans{tr=with_english(Tr, Source)}.

%% @doc Translate a message in a gettext context using the request languages.
-spec trans_msgctxt(Text, MsgCtxt, Context) -> binary() when
    Text :: binary() | string(), MsgCtxt :: binary() | string(), Context :: z:context().
trans_msgctxt(Text, MsgCtxt, Context) ->
    trans_msgctxt(Text, MsgCtxt, z_context:languages(Context), Context).

%% @doc Translate within a message context, with an explicit language selection.
-spec trans_msgctxt(Text, MsgCtxt, Languages, Context) -> binary() when
    Text :: binary() | string(), MsgCtxt :: binary() | string(),
    Languages :: atom() | [atom()], Context :: z:context().
trans_msgctxt(Text, MsgCtxt, Language, Context) when is_atom(Language) ->
    trans_msgctxt(Text, MsgCtxt, [Language], Context);
trans_msgctxt(Text, MsgCtxt, Languages, Context) ->
    #trans{tr=Tr} = translations(Text, MsgCtxt, Context),
    case find_first(Languages, Tr) of
        undefined -> proplists:get_value(en, Tr);
        Translation -> Translation
    end.

catalog_translations(Key, Context) ->
    try ets:lookup(z_trans_server:table(Context), Key) of
        [{_, Tr}] -> Tr;
        [] -> []
    catch
        error:badarg -> []
    end.

contextual_translations(Key, MsgCtxt, Fallback, Context) ->
    Contextual = catalog_translations({context, MsgCtxt, Key}, Context),
    case Fallback of
        false -> Contextual;
        true ->
            General = catalog_translations(Key, Context),
            Combined = lists:foldl(fun({Lang, Value}, Acc) ->
                case maps:find(Lang, Acc) of
                    {ok, GeneralValue} when is_tuple(Key) ->
                        % Rules may differ between catalogs: do not merge form indices.
                        Acc#{Lang => {plural_candidates, [Value, GeneralValue]}};
                    _ when Value =:= <<>> -> Acc;
                    _ -> Acc#{Lang => Value}
                end
            end, maps:from_list(General), Contextual),
            lists:sort(maps:to_list(Combined))
    end.

with_english(Tr, Source) ->
    case proplists:is_defined(en, Tr) of
        true -> Tr;
        false -> [{en, Source}|Tr]
    end.

%% @doc All plural translations, keyed separately from singular messages.
%% Forms and their parsed rule stay together through language fallback.
-spec plural_translations(Singular, Plural, Context) -> #trans{} when
    Singular :: binary() | string(), Plural :: binary() | string(), Context :: z:context().
plural_translations(Singular, Plural, Context) when is_binary(Singular), is_binary(Plural) ->
    plural_translations_key({plural, Singular, Plural}, Singular, Plural, Context);
plural_translations(Singular, Plural, Context) ->
    plural_translations(unicode:characters_to_binary(Singular), unicode:characters_to_binary(Plural), Context).

%% @doc Fetch plural forms in a gettext message context. No fallback to a
%% different message context: its grammatical case or meaning can be different.
-spec plural_translations(Singular, Plural, MsgCtxt, Context) -> #trans{} when
    Singular :: binary() | string(), Plural :: binary() | string(),
    MsgCtxt :: binary() | string(), Context :: z:context().
plural_translations(Singular, Plural, MsgCtxt, Context) ->
    plural_translations(Singular, Plural, MsgCtxt, false, Context).

%% @doc Optionally try a contextless plural entry in the same language.
-spec plural_translations(Singular, Plural, MsgCtxt, Fallback, Context) -> #trans{} when
    Singular :: binary() | string(), Plural :: binary() | string(),
    MsgCtxt :: binary() | string(), Fallback :: boolean(), Context :: z:context().
plural_translations(Singular, Plural, MsgCtxt, Fallback, Context) when is_boolean(Fallback) ->
    S = unicode:characters_to_binary(Singular),
    P = unicode:characters_to_binary(Plural),
    Tr = contextual_translations({plural, S, P}, unicode:characters_to_binary(MsgCtxt), Fallback, Context),
    #trans{tr=with_english(Tr, {template_compiler_plural:english(), {S, P}})}.

plural_translations_key(Key, Singular, Plural, Context) ->
    Tr = try ets:lookup(z_trans_server:table(Context), Key) of
        [{_, Trans}] -> Trans;
        [] -> []
    catch
        error:badarg -> []
    end,
    case proplists:is_defined(en, Tr) of
        true -> #trans{tr=Tr};
        false -> #trans{tr=[{en, {template_compiler_plural:english(), {Singular, Plural}}}|Tr]}
    end.

%% @doc Translate a plural message for a non-negative integer count. Like trans/2,
%% this returns text; callers interpolate placeholders separately.
-spec trans_plural(Singular, Plural, N, Context) -> binary() when
    Singular :: binary() | string(), Plural :: binary() | string(), N :: non_neg_integer(), Context :: z:context().
trans_plural(Singular, Plural, N, Context) when is_binary(Singular), is_binary(Plural) ->
    Trans = plural_translations(Singular, Plural, Context),
    Selected = lookup_fallback(Trans, Context),
    template_compiler_plural:select(Selected, N,
        {template_compiler_plural:english(), {Singular, Plural}});
trans_plural(Singular, Plural, N, Context) ->
    trans_plural(unicode:characters_to_binary(Singular), unicode:characters_to_binary(Plural), N, Context).

%% @doc Select a plural form within a gettext message context.
-spec trans_plural(Singular, Plural, N, MsgCtxt, Context) -> binary() when
    Singular :: binary() | string(), Plural :: binary() | string(),
    N :: non_neg_integer(), MsgCtxt :: binary() | string(), Context :: z:context().
trans_plural(Singular, Plural, N, MsgCtxt, Context) ->
    S = unicode:characters_to_binary(Singular),
    P = unicode:characters_to_binary(Plural),
    Trans = plural_translations(S, P, MsgCtxt, Context),
    template_compiler_plural:select(lookup_fallback(Trans, Context), N,
        {template_compiler_plural:english(), {S, P}}).

merge_trs([], Acc) ->
    lists:reverse(Acc);
merge_trs([{Lang,_} = LT|Rest], Acc) ->
    case proplists:is_defined(Lang, Acc) of
        true -> merge_trs(Rest, Acc);
        false -> merge_trs(Rest, [LT|Acc])
    end.

%% @doc Prepare a translations table based on all .po files in the active modules.
%%      Returns a map of English sentences with all their translations
-spec parse_translations(z:context()) -> map().
parse_translations(Context) ->
    Mods = z_module_indexer:translations(Context),
    ParsePOFiles = parse_mod_pofiles(Mods, []),
    build_index(ParsePOFiles, #{}).

%% @doc Parse all .po files. Results in a map with per found label
%% a list of translations: [iso_code, trans]. Usually, the labels
%% are the English sentences.
parse_mod_pofiles([], Acc) ->
    lists:reverse(Acc);
parse_mod_pofiles([{_Module, POFiles}|Rest], Acc) ->
    Acc1 = parse_pofiles(POFiles, Acc),
    parse_mod_pofiles(Rest, Acc1).

parse_pofiles([], Acc) ->
    Acc;
parse_pofiles([{Lang,File}|POFiles], Acc) ->
    parse_pofiles(POFiles, [{Lang, z_gettext:parse_po(File)}|Acc]).

build_index([], Acc) ->
    Acc;
build_index([{Lang, Labels}|Rest], Acc) ->
    build_index(Rest, add_labels(Lang, Labels, Acc)).

add_labels(_Lang, [], Acc) ->
    Acc;
add_labels(Lang, [{header,_}|Rest], Acc) ->
    add_labels(Lang, Rest, Acc);
add_labels(Lang, [{_Label, {undefined, _}}|Rest], Acc) ->
    % No usable catalog rule: use the source language instead of guessing.
    add_labels(Lang, Rest, Acc);
add_labels(Lang, [{Label, Trans}|Rest], Acc) when is_tuple(Label) ->
    Ts = maps:get(Label, Acc, []),
    case proplists:is_defined(Lang, Ts) of
        true -> add_labels(Lang, Rest, Acc);
        false -> add_labels(Lang, Rest, Acc#{Label => [{Lang, Trans}|Ts]})
    end;
add_labels(Lang, [{Label,Trans}|Rest], Acc) when is_binary(Trans), is_binary(Label) ->
    case maps:find(Label, Acc) of
        {ok, Ts} ->
            case proplists:is_defined(Lang, Ts) of
                false -> add_labels(Lang, Rest, Acc#{ Label => [{Lang,Trans}|Ts]});
                true -> add_labels(Lang, Rest, Acc)
            end;
        error ->
            add_labels(Lang, Rest, Acc#{ Label => [{Lang,Trans}] })
    end.

%% @doc Strict translation lookup of a language version using the languages
%% in the context. If no translation is found or the input is undefined, then
%% undefined is returned.
-spec lookup(Text, Context) -> Translation when
    Text :: z:trans() | binary() | string() | undefined,
    Context :: z:context(),
    Translation :: binary() | undefined.
lookup(Trans, Context) ->
    lookup(Trans, z_context:languages(Context), Context).

%% @doc Strict translation lookup of a language version using the given language
%% or languages. If no translation is found or the input is undefined, then
%% undefined is returned.
-spec lookup(Text, Language, Context) -> Translation | undefined when
    Text :: z:trans() | binary() | string() | undefined,
    Language :: z_language:language_code() | [ z_language:language_code() ],
    Context :: z:context(),
    Translation :: binary().
lookup(undefined, _Lang, _Context) ->
    undefined;
lookup(Text, Lang, Context) when is_list(Text) ->
    lookup(unicode:characters_to_binary(Text), Lang, Context);
lookup(#trans{ tr = Tr }, Lang, _Context) when is_atom(Lang) ->
    case lists:keyfind(Lang, 1, Tr) of
        {_, T} -> T;
        false -> undefined
    end;
lookup(Text, Lang, Context) when is_atom(Lang), is_binary(Text) ->
     case z_context:language(Context) of
        Lang -> Text;
        _ -> undefined
    end;
lookup(Text, Lang, Context) when is_atom(Lang) ->
    lookup(Text, [Lang], Context);
lookup(#trans{ tr = Tr }, Langs, _Context) ->
    find_first(Langs, Tr);
lookup(Text, Langs, Context) when is_binary(Text) ->
    ContextLangs = z_context:languages(Context),
    case lists:any(fun(Iso) -> lists:member(Iso, ContextLangs) end, Langs) of
        true -> Text;
        false -> undefined
    end;
lookup(_Text, _Langs, _Context) ->
    undefined.

%% @doc Non strict translation lookup of a language version.
%% In order check: requested languages, default configured language, english, any
%% If no translation is found, then the empty binary string is returned.
-spec lookup_fallback(Text, OptContext) -> Translation when
    Text :: z:trans() | binary() | string() | list() | undefined,
    OptContext :: z:context() | undefined,
    Translation :: binary().
lookup_fallback(undefined, _OptContext) ->
    <<>>;
lookup_fallback(Trans, undefined) ->
    lookup_fallback(Trans, [en], undefined);
lookup_fallback(Trans, Context) ->
    lookup_fallback(Trans, z_context:languages(Context), Context).

%% @doc Non strict translation lookup of a language version.
%% In order check: requested languages, default configured language, english, any
%% If no translation is found, then the empty binary string is returned.
-spec lookup_fallback(Text, Language, OptContext) -> Translation when
    Text :: z:trans() | binary() | string() | list() | undefined,
    Language :: z_language:language_code() | [ z_language:language_code() ] | binary(),
    OptContext :: z:context() | undefined,
    Translation :: binary().
lookup_fallback(TextList, Lang, OptContext) when is_list(TextList) ->
    case is_iodata(TextList) of
        true ->
            Text1 = unicode:characters_to_binary(TextList, utf8),
            lookup_fallback(Text1, Lang, OptContext);
        false ->
            T1 = lists:map(
                fun(T) -> lookup_fallback(T, Lang, OptContext) end,
                TextList),
            case unicode:characters_to_binary(T1, utf8) of
                S when is_binary(S) -> S;
                _ -> <<>>
            end
    end;
lookup_fallback(Text, Lang, OptContext) when is_atom(Lang) ->
    lookup_fallback(Text, [Lang], OptContext);
lookup_fallback(Text, Lang, OptContext) when is_binary(Lang) ->
    case z_language:to_language_atom(Lang) of
        {ok, Code} ->
            lookup_fallback(Text, [Code], OptContext);
        {error, _} when is_binary(Text) ->
            Text;
        {error, _} ->
            <<>>
    end;
lookup_fallback(#trans{ tr = Tr }, Langs, OptContext) when is_list(Langs) ->
    case find_first(Langs, Tr) of
        undefined ->
            CfgLang = z_language:default_language(OptContext),
            case lists:keyfind(CfgLang, 1, Tr) of
                {_, Text} -> Text;
                false -> take_english_or_first(Tr)
            end;
        Text ->
            Text
    end;
lookup_fallback(Text, _Lang, _Context) when is_binary(Text) ->
    Text;
lookup_fallback(_Text, _Lang, _Context) ->
    <<>>.

is_iodata([H|T]) when is_integer(H), H >= 1 -> is_iodata(T);
is_iodata([H|T]) when is_binary(H) -> is_iodata(T);
is_iodata([H|T]) when is_list(H) -> is_iodata(H) andalso is_iodata(T);
is_iodata([]) -> true;
is_iodata(_) -> false.

-spec find_first(Langs, Texts) -> binary() | undefined when
    Langs :: [ z_language:language_code() ],
    Texts :: [ {z_language:language_code(), binary()} ].
find_first(_Langs, []) ->
    undefined;
find_first([], _Tr) ->
    undefined;
find_first([Lang|Langs], Tr) ->
    case lists:keyfind(Lang, 1, Tr) of
        false -> find_first(Langs, Tr);
        {_, Text} -> Text
    end.

-spec take_english_or_first(Tr) -> binary() when
    Tr :: [ {z_language:language_code(), binary()} ].
take_english_or_first(Tr) ->
    case lists:keyfind(en, 1, Tr) of
        false ->
            case Tr of
                [{_,Text}|_] -> Text;
                _ -> <<>>
            end;
        {_, Text} ->
            Text
    end.


%% @doc Return the language that would be selected, given the context languages.
-spec lookup_fallback_languages(Available, Context) -> Language when
    Available :: [ atom() ],
    Context :: z:context(),
    Language :: atom().
lookup_fallback_languages([], Context) ->
    z_context:language(Context);
lookup_fallback_languages(Available, Context) ->
    Enabled = z_context:languages(Context),
    case lookup_fallback_languages_1(Enabled, Available) of
        undefined ->
            case lists:member(en, Available) of
                true -> en;
                false -> hd(Available)
            end;
        Lang ->
            Lang
    end.

lookup_fallback_languages_1([], _Available) ->
    undefined;
lookup_fallback_languages_1([Lang|Enabled], Available) ->
    case lists:member(Lang, Available) of
        true -> Lang;
        false -> lookup_fallback_languages_1(Enabled, Available)
    end.


-spec lookup_fallback_language([atom()], z:context()) -> atom().
lookup_fallback_language(Langs, Context) ->
    lookup_fallback_language(Langs, z_context:language(Context), Context).

-spec lookup_fallback_language([atom()], atom(), z:context()) -> atom().
lookup_fallback_language([], Lang, _Context) ->
    Lang;
lookup_fallback_language(Langs, Lang, Context) ->
    case lists:member(Lang, Langs) of
        false ->
            case z_language:default_language(Context) of
                undefined ->
                    case lists:member(en, Langs) of
                        true ->
                            en;
                        false ->
                            case Langs of
                                [] -> Lang;
                                [L|_] -> L
                            end
                    end;
                CfgLang ->
                    CfgLangAtom = z_convert:to_atom(CfgLang),
                    case lists:member(CfgLangAtom, Langs) of
                        false ->
                            case lists:member(en, Langs) of
                                true ->
                                    en;
                                false ->
                                    case Langs of
                                        [] -> Lang;
                                        [L|_] -> L
                                    end
                            end;
                        true ->
                            CfgLangAtom
                    end
            end;
        true ->
            Lang
    end.


%% @doc translate a string or trans record into another language. Prefer
%% the language translations from the .po files over the translations in
%% the trans record itself.
-spec trans(Text, ContextOrLanguage) -> Translation when
    Text :: z:trans() | binary() | string(),
    ContextOrLanguage :: z:context() | [ atom() ] | atom(),
    Translation :: binary().
trans(Text, Lang) when is_list(Text) ->
    trans(unicode:characters_to_binary(Text), Lang);
trans(Text, #context{} = Context) when is_binary(Text) ->
    trans(Text, z_context:languages(Context), Context);
trans(#trans{} = Trans, #context{} = Context) ->
    trans(Trans, z_context:languages(Context));
trans(#trans{ tr = Tr }, Langs) ->
    case find_first(Langs, Tr) of
        undefined -> take_english_or_first(Tr);
        T -> T
    end;
trans(Text, _Lang) when is_binary(Text) ->
    Text;
trans(_Text, _Lang) ->
    <<>>.


-spec trans(Text, Language, Context) -> Translation when
    Text :: z:trans() | binary() | string(),
    Language :: [ atom() ] | atom(),
    Context :: z:context(),
    Translation :: binary().
trans(Text, Language, Context) when is_atom(Language) ->
    trans(Text, [ Language ], Context);
trans(Text, Languages, Context) when is_list(Text) ->
    trans(unicode:characters_to_binary(Text), Languages, Context);
trans(#trans{ tr = [] }, _Languages, _Context) ->
    <<>>;
trans(#trans{ tr = Tr0 }, Languages, Context) ->
    case lists:keyfind(en, 1, Tr0) of
        {en, EnText} ->
            #trans{ tr = Tr } = translations(EnText, Context),
            case find_first(Languages, Tr) of
                undefined ->
                    case find_first(Languages, Tr0) of
                        undefined -> EnText;
                        T -> T
                    end;
                T ->
                    T
            end;
        false ->
            case find_first(Languages, Tr0) of
                undefined -> <<>>;
                T -> T
            end
    end;
trans(Text, Languages, Context) when is_binary(Text) ->
    #trans{ tr = Tr } = translations(Text, Context),
    case find_first(Languages, Tr) of
        undefined -> Text;
        T -> T
    end;
trans(_Text, _Languages, _Context) ->
    <<>>.
