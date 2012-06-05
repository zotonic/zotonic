%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Translate english sentences into other languages, following
%% the GNU gettext principle.

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

-module(z_trans).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    translations/2,
    parse_translations/1,
    trans/2,
    lookup/2,
    lookup/3,
    lookup_fallback/2,
    lookup_fallback/3,
    lookup_fallback_language/2,
    lookup_fallback_language/3,
    default_language/1, 
    is_language/1, 
    to_language_atom/1, 
    lc2descr/1
]).

-include_lib("zotonic.hrl").


%% @doc Fetch all translations for the given string.
%% @spec translations(From, Context) -> #trans{} | binary()
translations({trans, Tr0} = Trans0, Context) ->
    {en, From} = proplists:lookup(en, Tr0),
    case translations(From, Context) of
        {trans, Tr1} -> merge_trs(Tr0, Tr1);
        _ -> Trans0
    end;
translations(From, Context) when is_binary(From) ->
    case ets:lookup(z_trans_server:table(Context), From) of
        [] ->
			From;
        [{_, Trans}] ->
			{trans, Trans}
    end;
translations(From, Context) ->
    translations(z_convert:to_binary(From), Context).

    merge_trs([], Acc) ->
        Acc;
    merge_trs([{Lang,_} = LT|Rest], Acc) ->
        case proplists:is_defined(Lang, Acc) of
            true -> merge_trs(Rest, Acc);
            false -> merge_trs(Rest, [LT|Acc])
        end.

%% @doc Prepare a translations table based on all .po files in the active modules.
parse_translations(Context) ->
    Mods = z_module_indexer:translations(Context),
    build_index(parse_mod_trans(Mods, []), dict:new()).

    %% @doc Parse all .po files. Results in a dict {label, [iso_code,trans]}
    parse_mod_trans([], Acc) ->
        lists:reverse(Acc);
    parse_mod_trans([{_Module, POList}|Rest], Acc) ->
        Acc1 = parse_trans(POList, Acc),
        parse_mod_trans(Rest, Acc1).
    
    parse_trans([], Acc) ->
        Acc;
    parse_trans([{Lang,File}|Rest], Acc) ->
        parse_trans(Rest, [{Lang, z_gettext:parse_po(File)}|Acc]).
    
    build_index([], Dict) ->
        Dict;
    build_index([{Lang, Labels}|Rest], Dict) ->
        build_index(Rest, add_labels(Lang, Labels, Dict)).
    
    add_labels(_Lang, [], Dict) ->
        Dict;
    add_labels(Lang, [{header,_}|Rest],Dict) ->
        add_labels(Lang, Rest,Dict);
    add_labels(Lang, [{Label,Trans}|Rest], Dict) ->
        LabelB = list_to_binary(Label),
        case dict:find(LabelB, Dict) of
            {ok, Ts} ->
                case proplists:get_value(Lang, Ts) of
                    undefined -> add_labels(Lang, Rest, dict:store(LabelB, [{Lang,list_to_binary(Trans)}|Ts], Dict));
                    _PrevTrans -> add_labels(Lang, Rest, Dict)
                end;
            error -> 
                add_labels(Lang, Rest, dict:store(LabelB,[{Lang,to_binary(Trans)}],Dict))
        end.
        
        to_binary(header) -> "";
        to_binary(L) -> list_to_binary(L).
            


%% @doc Strict translation lookup of a language version
lookup(Trans, Context) ->
    lookup(Trans, z_context:language(Context), Context).
    
lookup({trans, Tr}, Lang, _Context) ->
    proplists:get_value(Lang, Tr);
lookup(Text, Lang, Context) ->
    case z_context:language(Context) of
        Lang -> Text;
        _ -> undefined
    end.

%% @doc Non strict translation lookup of a language version.
%%      In order check: requested language, default configured language, english, any
lookup_fallback(Trans, Context) ->
    lookup_fallback(Trans, z_context:language(Context), Context).

lookup_fallback({trans, Tr}, Lang, Context) ->
    case proplists:get_value(Lang, Tr) of
        undefined ->
            case default_language(Context) of
                undefined -> take_english_or_first(Tr);
                CfgLang ->
                    case proplists:get_value(z_convert:to_atom(CfgLang), Tr) of
                        undefined -> take_english_or_first(Tr);
                        Text -> Text
                    end
            end;
        Text -> 
            Text
    end;
lookup_fallback(Text, _Lang, _Context) ->
    Text.
    
    take_english_or_first(Tr) ->
        case proplists:get_value(en, Tr) of
            undefined ->
                case Tr of
                    [{_,Text}|_] -> Text;
                    _ -> undefined
                end;
            EnglishText -> 
                EnglishText
        end.


lookup_fallback_language(Langs, Context) ->
    lookup_fallback_language(Langs, z_context:language(Context), Context).

lookup_fallback_language([], Lang, _Context) ->
    Lang;
lookup_fallback_language(Langs, Lang, Context) ->
    case lists:member(Lang, Langs) of
        false ->
            case default_language(Context) of
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


%% @doc translate a string or trans record into another language
%% @spec trans(From, Language) -> String
%%   From = #trans{} | String
%%   Language = atom()
trans({trans, Tr}, Lang) when is_atom(Lang) ->
    proplists:get_value(Lang, Tr);
trans(Text, Lang) when is_atom(Lang) ->
    Text;
trans(Text, Context) ->
    trans(Text, Context#context.language, Context).

trans({trans, Tr0}, Language, Context) ->
    case proplists:lookup(en, Tr0) of
        {en, Text} ->
            case translations(Text, Context) of
                {trans, Tr} ->
                    case proplists:get_value(Language, Tr) of
                        undefined -> proplists:get_value(Language, Tr0, Text);
                        Translated -> Translated
                    end;
                _ ->
                    proplists:get_value(Language, Tr0, Text)
            end;
        none ->
            proplists:get_value(Language, Tr0)
    end;
trans(Text, Language, Context) ->
    case translations(Text, Context) of
        {trans, Tr} ->
            proplists:get_value(Language, Tr, Text);
        _ -> Text
    end.


%% @doc Return the configured default language for this server
-spec default_language(#context{}) -> atom().
default_language(Context) ->
    z_convert:to_atom(m_config:get_value(i18n, language, en, Context)).


%% @doc check if the two letter code is a valid language
-spec is_language(Language :: term()) -> boolean().
is_language(<<A,B>>) -> iso639:lc2lang([A,B]) /= "";
is_language([_,_] = IsoCode) -> iso639:lc2lang(IsoCode) /= "";
is_language(_) -> false.

%% @doc Translate a language-code to an atom.
-spec to_language_atom(IsoCode:: list() | binary()) -> {ok, atom()} | {error, not_a_language}.
to_language_atom([_,_] = IsoCode) ->
    case is_language(IsoCode) of
        false -> {error, not_a_language};
        true -> {ok, list_to_atom(IsoCode)}
    end;
to_language_atom(IsoCode) when is_atom(IsoCode) ->
    to_language_atom(atom_to_list(IsoCode));
to_language_atom(IsoCode) when is_binary(IsoCode) ->
    to_language_atom(binary_to_list(IsoCode));
to_language_atom(_) ->
    {error, not_a_language}.


%% @doc Return a descriptive (english) string for the language
%% @spec lc2descr(Language) -> Descr
%%  Language = atom()
%%  Descr = list()
lc2descr(Language) ->
    iso639:lc2lang(atom_to_list(Language)).
