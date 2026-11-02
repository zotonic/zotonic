%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Prepare admin dropdown entries and separators.
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

-module(filter_admin_menu_items).
-moduledoc("Flatten admin dropdown subgroups into headings and links, with separators
around groups. Remove leading, trailing and consecutive separators after flattening,
including those contributed explicitly by modules or nested groups.").

-export([admin_menu_items/2]).

-spec admin_menu_items(Items, Context) -> list() when
    Items :: list() | undefined,
    Context :: z:context().
admin_menu_items(undefined, _Context) ->
    [];
admin_menu_items(Items, _Context) ->
    normalize(lists:flatten(flatten(Items)), true).

flatten(Items) ->
    lists:map(
        fun({Id, Props} = Item) ->
            case proplists:get_value(items, Props, []) of
                [] ->
                    Item;
                Children ->
                    [separator(), {Id, [{header, true} | Props]},
                     flatten(Children), separator()]
            end
        end,
        Items).

separator() ->
    {undefined, [{separator, true}]}.

%% Start as if preceded by a separator, so leading separators are skipped.
normalize([], _PreviousSeparator) ->
    [];
normalize([{_, Props} = Item | Rest], PreviousSeparator) ->
    case proplists:get_value(separator, Props, false) of
        true when PreviousSeparator ->
            normalize(Rest, true);
        true ->
            case normalize(Rest, true) of
                [] -> [];
                Items -> [Item | Items]
            end;
        IsSeparator ->
            [Item | normalize(Rest, IsSeparator)]
    end.
