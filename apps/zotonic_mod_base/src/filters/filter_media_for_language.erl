%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023 Marc Worrell
%% @doc Filter a list of media items by their 'medium_language' property,
%% return the best matching with the current or given language. Only visible
%% media items are returned.
%% @end

%% Copyright 2023 Marc Worrell
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

-module(filter_media_for_language).

-export([
    media_for_language/2,
    media_for_language/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

media_for_language(Ids, Context) ->
    media_1(Ids, Context).

media_for_language(Ids, Language, Context) ->
    Context1 = case z_language:to_language_atom(Language) of
        {ok, Code} ->
            z_context:set_language(Code, Context);
        {error, _} ->
            Context
    end,
    media_1(Ids, Context1).


media_1(Ids, Context) ->
    LangIds = lists:filtermap(
        fun(Id) ->
            case m_rsc:rid(Id, Context) of
                undefined ->
                    false;
                RId ->
                    case z_acl:rsc_visible(RId, Context) of
                        true ->
                            MediaLang = m_rsc:p(RId, <<"medium_language">>, Context),
                            Lang = case z_language:to_language_atom(MediaLang) of
                                {ok, Code} -> Code;
                                {error, _} -> 'x-default'
                            end,
                            {true, {Lang, RId}};
                        false ->
                            false
                    end
            end
        end,
        Ids),
    Grouped = lists:foldr(
        fun({Code, Id}, Acc) ->
            Acc#{
                Code => [ Id | maps:get(Code, Acc, []) ]
            }
        end,
        #{},
        LangIds),
    Tr = #trans{ tr = maps:to_list(Grouped) },
    case z_trans:lookup_fallback(Tr, Context) of
        undefined ->
            [];
        SelectedIds ->
            SelectedIds
    end.
