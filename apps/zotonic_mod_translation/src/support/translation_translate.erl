%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023-2025 Driebit BV
%% @doc Translate strings from one language to another. Try to use the .po files and
%% optional translation services provided by other modules.
%% @end

%% Copyright 2023-2025 Driebit BV
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

-module(translation_translate).
-author("Marc Worrell <marc@worrell.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").

-export([ translate/4 ]).

%% @doc Translate one or more strings from one language to another. The strings
%% can be trans records, a single string or a list of strings. If the source language
%% is 'en' then the .po files are consulted for a preferred translation.
%% Both the from and to language must be configured as editable languages.

-spec translate(FromLanguage, ToLanguage, Texts, Context) -> {ok, Translations} | {error, Reason} when
    FromLanguage :: z_language:language(),
    ToLanguage :: z_language:language(),
    Texts :: [ Text ] | Text,
    Text :: binary() | #trans{},
    Context :: z:context(),
    Translations :: [ binary() | undefined ],
    Reason :: term().
translate(FromLanguage, ToLanguage, Texts, Context) ->
    FromCode = to_lang(FromLanguage, Context),
    ToCode = to_lang(ToLanguage, Context),
    translate_1(FromCode, ToCode, to_list(Texts), Context).

translate_1({error, _}, _ToCode, _Texts, _Context) ->
    {error, from_language};
translate_1(_FromCode, {error, _}, _Texts, _Context) ->
    {error, to_language};
translate_1({ok, FromCode}, {ok, ToCode}, Texts, Context) ->
    PartlyTranslated = lists:map(
        fun(Text) ->
            local_trans(FromCode, ToCode, trim(Text), Context)
        end,
        Texts),
    add_service_trans(FromCode, ToCode, PartlyTranslated, Context).


add_service_trans(FromCode, ToCode, PartlyTranslated, Context) ->
    case lists:filtermap(
        fun
            ({FromText, undefined}) -> {true, FromText};
            (_) -> false
        end,
        PartlyTranslated)
    of
        [] ->
            Translations = [ T || {_, T} <- PartlyTranslated ],
            {ok, Translations};
        ToTranslate ->
            Notification = #translate{
                from = FromCode,
                to = ToCode,
                texts = ToTranslate
            },
            case z_notifier:first(Notification, Context) of
                {ok, Translations} ->
                    {ok, merge_translations(PartlyTranslated, Translations, [])};
                {error, _} = Error ->
                    Error;
                undefined ->
                    {_, Translations} = lists:unzip(PartlyTranslated),
                    {ok, Translations}
            end
    end.

merge_translations([], [], Acc) ->
    lists:reverse(Acc);
merge_translations([{_Text, undefined}|Rest], [Tr|Trs], Acc) ->
    merge_translations(Rest, Trs, [ Tr | Acc ]);
merge_translations([{_Text, Tr}|Rest], Trs, Acc) ->
    merge_translations(Rest, Trs, [ Tr | Acc ]).


local_trans(FromCode, ToCode, Text, _Context) when is_binary(Text), FromCode =:= ToCode ->
    {Text, Text};
local_trans(_FromCode, _ToCode, <<>>, _Context) ->
    {<<>>, <<>>};
local_trans(_FromCode, _ToCode, #trans{ tr = [] }, _Context) ->
    {<<>>, <<>>};
local_trans(FromCode, ToCode, #trans{ tr = Tr }, Context) ->
    FromCodeText = lists:keyfind(FromCode, 1, Tr),
    case lists:keyfind(ToCode, 1, Tr) of
        {_ToCode, ToText} ->
            {<<>>, ToText};
        false when FromCodeText =:= false ->
            {<<>>, <<>>};
        false when FromCode =:= en ->
            {en, FromText} = FromCodeText,
            case trans(FromText, ToCode, Context) of
                undefined -> {FromText, undefined};
                ToText -> {FromText, ToText}
            end;
        false ->
            {_FromCode, FromText} = FromCodeText,
            {FromText, undefined}
    end;
local_trans(en, ToCode, Text, Context) ->
    {Text, trans(Text, ToCode, Context)};
local_trans(_FromCode, _ToCode, Text, _Context) ->
    {Text, undefined}.

trans(Text, Language, Context) ->
    #trans{ tr = Tr } = z_trans:translations(Text, Context),
    case lists:keyfind(Language, 1, Tr) of
        {_, T} -> T;
        false -> undefined
    end.


to_list(#trans{} = Tr) ->
    [Tr];
to_list(Text) when is_binary(Text) ->
    [Text];
to_list(L) when is_list(L) ->
    L.

trim(Text) when is_binary(Text) -> z_string:trim(Text);
trim(Text) -> Text.

to_lang(Language, Context) ->
    case z_language:to_language_atom(Language) of
        {ok, Code} ->
            case z_language:is_language_editable(Code, Context) of
                true -> {ok, Code};
                false -> {error, unacceptable}
            end;
        {error, _} = Error ->
            Error
    end.
