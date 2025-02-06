%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2024 Driebit BV
%% @doc Translate a resource or property map, adds or removes languages.
%% @end

%% Copyright 2024 Driebit BV
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

-module(translation_translate_rsc).
-author("Marc Worrell <marc@worrell.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    has_language/3,

    add_translation/5,
    add_translation_map/5,

    remove_translation/3,
    remove_translation_map/2,

    keep_translation/3,
    keep_translation_map/2
]).


%% @doc Add a translation to the resource. The source and destination language must be editable
%% languages for the site. If the source language is not in the resource then an error is returned.
-spec add_translation(Id, FromLanguage, ToLanguage, IsOverwrite, Context) -> ok | {error, Reason} when
    Id :: m_rsc:resource(),
    FromLanguage :: z_language:language_code(),
    ToLanguage :: z_language:language_code(),
    IsOverwrite :: boolean(),
    Context :: z:context(),
    Reason :: term().
add_translation(Id, FromLanguage, ToLanguage, IsOverwrite, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            case {z_language:is_language_editable(FromLanguage, Context),
                  z_language:is_language_editable(ToLanguage, Context)}
            of
                {false, _} ->
                    {error, src};
                {_, false} ->
                    {error, dst};
                {true, true} ->
                    case has_language(Id, FromLanguage, Context) of
                        true ->
                            translate_1(Id, FromLanguage, ToLanguage, IsOverwrite, Context);
                        false ->
                            {error, nosrc}
                    end
            end;
        false ->
            {error, eacces}
    end.


%% @doc Add a translation to a map. The source and destination language must be editable
%% languages for the site.
-spec add_translation_map(Map, FromLanguage, ToLanguage, IsOverwrite, Context) -> {ok, NewMap} | {error, Reason} when
    Map :: map(),
    NewMap :: map(),
    FromLanguage :: z_language:language_code(),
    ToLanguage :: z_language:language_code(),
    IsOverwrite :: boolean(),
    Context :: z:context(),
    Reason :: term().
add_translation_map(Map, FromLanguage, ToLanguage, IsOverwrite, Context) ->
    Texts = collect_texts_1(Map, FromLanguage, ToLanguage, IsOverwrite, []),
    case m_translation:translate_to_lookup(FromLanguage, ToLanguage, Texts, Context) of
        {ok, Translations} ->
            TransMap = lists:foldl(
                fun(#{ <<"text">> := Txt, <<"translation">> := TxtTr }, Acc) ->
                    Acc#{
                        Txt => TxtTr
                    }
                end,
                #{},
                Translations),
            Map1 = insert_dst_texts_1(Map, FromLanguage, ToLanguage, TransMap, IsOverwrite, true),
            {ok, Map1};
        {error, _} = Error ->
            Error
    end.

%% @doc Check if a resource has a language or one of the languages.
-spec has_language(Rsc, Language, Context) -> boolean() when
    Rsc :: m_rsc:resource() | map(),
    Language :: z_language:language_code() | [ z_language:language_code() ],
    Context :: z:context().
has_language(Rsc, Language, Context) when is_atom(Language) ->
    has_language(Rsc, [ Language ], Context);
has_language(#{ <<"language">> := Language }, Langs, _Context) when is_list(Langs), is_list(Language) ->
    lists:any(
        fun(Lang) -> lists:member(Lang, Language) end,
        Langs);
has_language(Id, Langs, Context) when is_list(Langs) ->
    case m_rsc:p_no_acl(Id, <<"language">>, Context) of
        undefined ->
            true;
        Language when is_list(Language) ->
            lists:any(
                fun(Lang) -> lists:member(Lang, Language) end,
                Langs)
    end.

%% @doc Remove all translations except the given ones from a resource.
-spec keep_translation(Id, Language, Context) -> ok | {error, Reason} when
    Id :: m_rsc:resource(),
    Language :: z_language:language_code() | [ z_language:language_code() ],
    Context :: z:context(),
    Reason :: term().
keep_translation(Id, Language, Context) when is_atom(Language) ->
    keep_translation(Id, [Language], Context);
keep_translation(_Id, [], _Context) ->
    {error, language};
keep_translation(Id, KeepLangs, Context) when is_list(KeepLangs) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            case has_language(Id, KeepLangs, Context) of
                true ->
                    RscLanguage = case m_rsc:p_no_acl(Id, <<"language">>, Context) of
                        undefined -> [];
                        Lng -> Lng
                    end,
                    RemoveLangs = RscLanguage -- KeepLangs,
                    remove_translation(Id, RemoveLangs, Context);
                false ->
                    {error, language}
            end;
        false ->
            {error, eacces}
    end.

%% @doc Remove all translations except the given ones from a map.
-spec keep_translation_map(Map, Language) -> {ok, NewMap} when
    Map :: map(),
    NewMap :: map(),
    Language :: z_language:language_code() | [ z_language:language_code() ].
keep_translation_map(Map, Language) when is_atom(Language) ->
    keep_translation_map(Map, [Language]);
keep_translation_map(Map, KeepLangs) when is_list(KeepLangs) ->
    MapLangs = collect_langs(Map),
    RemoveLangs = MapLangs -- KeepLangs,
    {ok, remove_1(Map, RemoveLangs, true, true)}.

%% @doc Remove languages from a resource.
-spec remove_translation(Id, Language, Context) -> ok | {error, Reason} when
    Id :: m_rsc:resource(),
    Language :: z_language:language_code() | [ z_language:language_code() ],
    Context :: z:context(),
    Reason :: term().
remove_translation(Id, Language, Context) when is_atom(Language) ->
    remove_translation(Id, [ Language ], Context);
remove_translation(_Id, [], _Context) ->
    ok;
remove_translation(Id, Langs, Context) when is_list(Langs) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            case m_rsc:exists(Id, Context) of
                true ->
                    case has_language(Id, Langs, Context) of
                        true ->
                            case m_rsc:get(Id, Context) of
                                undefined ->
                                    {error, enoent};
                                Rsc ->
                                    Rsc1 = case Rsc of
                                        #{ <<"language">> := _ } -> Rsc;
                                        _ -> Rsc#{ <<"language">> => [] }
                                    end,
                                    Rsc2 = case Rsc1 of
                                        #{ <<"translation_status">> := _ } -> Rsc;
                                        _ -> Rsc#{ <<"translation_status">> => #{} }
                                    end,
                                    Props = remove_1(Rsc2, Langs, false, true),
                                    case m_rsc:update(Id, Props, Context) of
                                        {ok, _} -> ok;
                                        {error, _} = Error -> Error
                                    end
                                end;
                        false ->
                            ok
                    end;
                false ->
                    {error, enoent}
            end;
        false ->
            {error, eacces}
    end.

%% @doc Remove languages from a map.
-spec remove_translation_map(Map, Language) -> {ok, NewMap} when
    Map :: map(),
    NewMap :: map(),
    Language :: z_language:language_code() | [ z_language:language_code() ].
remove_translation_map(Map, Language) when is_atom(Language) ->
    remove_translation_map(Map, [ Language ]);
remove_translation_map(Map, Langs) when is_list(Langs) ->
    {ok, remove_1(Map, Langs, true, true)}.

remove_1(V, [], true, _IsTopLevel) ->
    V;
remove_1(M, [], false, true) when is_map(M) ->
    #{};
remove_1(Map, Langs, IsCopyAll, IsTopLevel) when is_map(Map) ->
    maps:fold(
        fun
            (<<"language">>, V, Acc) when is_list(V), IsTopLevel ->
                Acc#{
                    <<"language">> => V -- Langs
                };
            (<<"translation_status">>, V, Acc) when is_map(V), IsTopLevel ->
                LangsB = [ atom_to_binary(Iso) || Iso <- Langs ],
                Acc#{
                    <<"translation_status">> => maps:without(LangsB, V)
                };
            (K, V, Acc) ->
                case remove_1(V, Langs, true, false) of
                    V when not IsCopyAll -> Acc;
                    V1 -> Acc#{ K => V1 }
                end
        end,
        #{},
        Map);
remove_1(L, Langs, _IsCopyAll, _IsTopLevel) when is_list(L) ->
    lists:map(
        fun(V) -> remove_1(V, Langs, true, false) end,
        L);
remove_1(#trans{ tr = Tr }, Langs, _IsCopyAll, _IsTopLevel) ->
    Tr1 = lists:foldl(
        fun(Lang, Acc) ->
            lists:keydelete(Lang, 1, Acc)
        end,
        Tr,
        Langs),
    #trans{ tr = Tr1 };
remove_1(V, _Language, _IsCopyAll, _IsTopLevel) ->
    V.


translate_1(Id, FromLanguage, ToLanguage, IsOverwrite, Context) ->
    Texts = collect_src_texts(Id, FromLanguage, ToLanguage, IsOverwrite, Context),
    case m_translation:translate_to_lookup(FromLanguage, ToLanguage, Texts, Context) of
        {ok, Translations} ->
            TransMap = lists:foldl(
                fun(#{ <<"text">> := Txt, <<"translation">> := TxtTr }, Acc) ->
                    Acc#{
                        Txt => TxtTr
                    }
                end,
                #{},
                Translations),
            Props = insert_dst_texts(Id, FromLanguage, ToLanguage, TransMap, IsOverwrite, Context),
            case maps:size(Props) of
                0 ->
                    ok;
                _ ->
                    TransStatus = case m_rsc:p_no_acl(Id, <<"translation_status">>, Context) of
                        undefined -> #{};
                        TrSt -> TrSt
                    end,
                    ToLanguageB = z_convert:to_binary(ToLanguage),
                    TransStatus1 = TransStatus#{ ToLanguageB => <<"1">> },
                    Language = case m_rsc:p_no_acl(Id, <<"language">>, Context) of
                        undefined -> [];
                        Lng -> Lng
                    end,
                    Language1 = case lists:member(ToLanguage, Language) of
                        true -> Language;
                        false -> [ ToLanguage | Language ]
                    end,
                    Props1 = Props#{
                        <<"translation_status">> => TransStatus1,
                        <<"language">> => lists:usort(Language1)
                    },
                    case m_rsc:update(Id, Props1, Context) of
                        {ok, _} -> ok;
                        {error, _} = Error -> Error
                    end
            end;
        {error, _} = Error ->
            Error
    end.

collect_src_texts(Id, FromLanguage, ToLanguage, IsOverwrite, Context) ->
    Rsc = m_rsc:get(Id, Context),
    collect_texts_1(Rsc, FromLanguage, ToLanguage, IsOverwrite, []).

collect_texts_1(Map, FromLanguage, ToLanguage, IsOverwrite, Acc0) when is_map(Map) ->
    maps:fold(
        fun(K, V, Acc) ->
            collect_texts_2(K, V, FromLanguage, ToLanguage, IsOverwrite, Acc)
        end,
        Acc0,
        Map);
collect_texts_1(L, FromLanguage, ToLanguage, IsOverwrite, Acc0) when is_list(L) ->
    lists:foldl(
        fun(V, Acc) ->
           collect_texts_2(<<>>, V, FromLanguage, ToLanguage, IsOverwrite, Acc)
        end,
        Acc0,
        L);
collect_texts_1(_, _FromLanguage, _ToLanguage, _IsOverwrite, Acc) ->
    Acc.

collect_texts_2(K, V, FromLanguage, ToLanguage, IsOverwrite, Acc) when is_binary(V) ->
    case is_text(K) of
        true ->
            V1 = #trans{ tr = [ {FromLanguage, V} ] },
            collect_trans(K, V1, FromLanguage, ToLanguage, IsOverwrite, Acc);
        false ->
            Acc
    end;
collect_texts_2(K, #trans{} = V, FromLanguage, ToLanguage, IsOverwrite, Acc) ->
    collect_trans(K, V, FromLanguage, ToLanguage, IsOverwrite, Acc);
collect_texts_2(_K, V, FromLanguage, ToLanguage, IsOverwrite, Acc) when is_list(V) ->
    collect_texts_1(V, FromLanguage, ToLanguage, IsOverwrite, Acc);
collect_texts_2(_K, V, FromLanguage, ToLanguage, IsOverwrite, Acc) when is_map(V) ->
    collect_texts_1(V, FromLanguage, ToLanguage, IsOverwrite, Acc);
collect_texts_2(_, _, _FromLanguage, _ToLanguage, _IsOverwrite, Acc) ->
    Acc.

collect_trans(K, #trans{ tr = Tr }, FromLanguage, ToLanguage, IsOverwrite, Acc) ->
    case is_json(K) of
        true ->
            Acc;
        false ->
            ToText = case lists:keyfind(ToLanguage, 1, Tr) of
                false -> <<>>;
                {_, <<>>} -> <<>>;
                {_, T} -> T
            end,
            case lists:keyfind(FromLanguage, 1, Tr) of
                {_, <<>>} ->
                    Acc;
                {_, FromText} when ToText =:= <<>> orelse IsOverwrite ->
                    [ FromText | Acc ];
                {_, _} ->
                    Acc;
                false ->
                    Acc
            end
    end.

is_json(K) ->
    binary:longest_common_suffix([K, <<"_json">>]) =:= 5.

% Fields we are sure of that they are text fields
is_text(<<"title">>) -> true;
is_text(<<"short_title">>) -> true;
is_text(<<"summary">>) -> true;
is_text(<<"body">>) -> true;
is_text(<<"body_extra">>) -> true;
is_text(<<"date_remarks">>) -> true;
is_text(<<"prompt">>) -> true;
is_text(<<"explanation">>) -> true;
is_text(<<"matching">>) -> true;
is_text(<<"narrative">>) -> true;
is_text(<<"feedback">>) -> true;
is_text(<<"seo_title">>) -> true;
is_text(<<"seo_desc">>) -> true;
is_text(<<"seo_keywords">>) -> true;
is_text(K) ->
    binary:longest_common_suffix([ K, <<"_html">> ]) =:= 5.

%% @doc Add the translation to all translatable texts.
insert_dst_texts(Id, FromLanguage, ToLanguage, Translations, IsOverwrite, Context) ->
    Rsc = m_rsc:get(Id, Context),
    insert_dst_texts_1(Rsc, FromLanguage, ToLanguage, Translations, IsOverwrite, false).

insert_dst_texts_1(Map, FromLanguage, ToLanguage, Translations, IsOverwrite, CopyAll) when is_map(Map) ->
    maps:fold(
        fun
            (<<"language">>, V, Acc) when is_list(V), CopyAll ->
                Acc#{
                    <<"language">> => lists:usort([ ToLanguage | V ])
                };
            (<<"translation_status">>, V, Acc) when is_map(V), CopyAll ->
                ToLanguageB = atom_to_binary(ToLanguage),
                Acc#{
                    <<"translation_status">> => V#{
                        ToLanguageB => <<"1">>
                    }
                };
            (K, V, Acc) when is_binary(V) ->
                case is_text(K) of
                    true ->
                        V1 = #trans{ tr = [ {FromLanguage, V} ] },
                        case dst_trans(K, V1, FromLanguage, ToLanguage, Translations, IsOverwrite) of
                            V1 when not CopyAll ->
                                Acc;
                            V2 ->
                                Acc#{ K => V2 }
                        end;
                    false when CopyAll ->
                        Acc#{ K => V };
                    false ->
                        Acc
                end;
            (K, #trans{} = V, Acc) ->
                case dst_trans(K, V, FromLanguage, ToLanguage, Translations, IsOverwrite) of
                    V when not CopyAll ->
                        Acc;
                    V1 ->
                        Acc#{ K => V1 }
                end;
            (K, L, Acc) when is_list(L) ->
                L1 = lists:map(
                    fun(V) ->
                        insert_dst_texts_1(V, FromLanguage, ToLanguage, Translations, IsOverwrite, true)
                    end,
                    L),
                case L1 of
                    L when not CopyAll -> Acc;
                    _ -> Acc#{ K => L1 }
                end;
            (K, M, Acc) when is_map(M) ->
                case insert_dst_texts_1(M, FromLanguage, ToLanguage, Translations, IsOverwrite, true) of
                    M when not CopyAll -> Acc;
                    M1 -> Acc#{ K => M1 }
                end;
            (K, V, Acc) when CopyAll ->
                Acc#{ K => V };
            (_, _, Acc) ->
                Acc
        end,
        #{},
        Map);
insert_dst_texts_1(L, FromLanguage, ToLanguage, Translations, IsOverwrite, _CopyAll) when is_list(L) ->
    lists:map(
        fun(V) ->
            insert_dst_texts_1(V, FromLanguage, ToLanguage, Translations, IsOverwrite, true)
        end,
        L);
insert_dst_texts_1(#trans{} = Tr, FromLanguage, ToLanguage, Translations, IsOverwrite, _CopyAll) ->
    dst_trans(<<>>, Tr, FromLanguage, ToLanguage, Translations, IsOverwrite);
insert_dst_texts_1(V, _FromLanguage, _ToLanguage, _Translations, _IsOverwrite, _CopyAll) ->
    V.

dst_trans(K, #trans{ tr = Tr } = V, FromLanguage, ToLanguage, Translations, IsOverwrite) ->
    case is_json(K) of
        false ->
            ToText = case lists:keyfind(ToLanguage, 1, Tr) of
                false -> <<>>;
                {_, <<>>} -> <<>>;
                {_, T} -> T
            end,
            if
                ToText =:= <<>> orelse IsOverwrite ->
                    Translated = case lists:keyfind(FromLanguage, 1, Tr) of
                        {_, FromText} ->
                            case maps:get(FromText, Translations, <<>>) of
                                undefined -> <<>>;
                                T1 -> T1
                            end;
                        false ->
                            V
                    end,
                    Tr1 = lists:keydelete(ToLanguage, 1, Tr),
                    Tr2 = [ {ToLanguage, Translated} | Tr1 ],
                    #trans{ tr = lists:sort(Tr2) };
                true ->
                    V
            end;
        true ->
            V
    end.

%% @doc Determine which languages are used in the data structure.
collect_langs(#{ <<"language">> := Langs }) when is_list(Langs) ->
    Langs;
collect_langs(Map) ->
    collect_langs_1(Map, []).

collect_langs_1(#trans{ tr = Tr }, Acc) ->
    Langs = [ Iso || {Iso, _} <- Tr ],
    lists:usort(Acc ++ Langs);
collect_langs_1(M, Acc) when is_map(M) ->
    maps:fold(
        fun(_K, V, Acc1) ->
            collect_langs_1(V, Acc1)
        end,
        Acc,
        M);
collect_langs_1(L, Acc) when is_list(L) ->
    lists:foldl(
        fun(V, Acc1) ->
            collect_langs_1(V, Acc1)
        end,
        Acc,
        L).
