%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Ordered, ACL-aware navigation through menus and collection descendants.
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

-module(z_rsc_sequence).
-moduledoc(<<"Ordered, ACL-aware navigation through menus and collection descendants.">>).
-export([adjacent/4]).
-include_lib("zotonic_core/include/zotonic.hrl").

-spec adjacent(Direction, Id, Container, Context) -> m_rsc:resource_id() | undefined when
    Direction :: next | prev,
    Id :: m_rsc:resource(),
    Container :: m_rsc:resource(),
    Context :: z:context().
adjacent(Direction, Id, Container, Context) ->
    case {m_rsc:rid(Id, Context), m_rsc:rid(Container, Context)} of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {RId, Root} ->
            {Ids, _} = walk([{Root, []}], #{}, [], Context),
            Ordered = case Direction of
                next -> lists:reverse(Ids);
                prev -> Ids
            end,
            following(RId, Ordered)
    end.

following(Id, [Id, Next | _]) -> Next;
following(Id, [_ | Rest]) -> following(Id, Rest);
following(_, []) -> undefined.

%% Keep explicit menu children ahead of linked descendants; visit each page once.
walk([], Seen, Acc, _Context) ->
    {Acc, Seen};
walk([{Ref, Children} | Rest], Seen, Acc, Context) ->
    case m_rsc:rid(Ref, Context) of
        undefined -> walk(Rest, Seen, Acc, Context);
        Id when is_map_key(Id, Seen) ->
            case maps:get(Id, Seen) of
                true -> walk(Children ++ Rest, Seen, Acc, Context);
                false -> walk(Rest, Seen, Acc, Context)
            end;
        Id ->
            Seen1 = Seen#{Id => true},
            case m_rsc:exists(Id, Context) andalso z_acl:rsc_visible(Id, Context) of
                false -> walk(Rest, Seen#{Id => false}, Acc, Context);
                true ->
                    Menu = menu(m_rsc:p(Id, <<"menu">>, Context)),
                    Parts = [{Part, []} || Part <- m_edge:objects(Id, haspart, Context)],
                    walk(Children ++ Menu ++ Parts ++ Rest, Seen1, [Id | Acc], Context)
            end
    end.

menu([#rsc_tree{id = Id, tree = Tree} | Rest]) ->
    [{Id, menu(Tree)} | menu(Rest)];
menu([{Id, Tree} | Rest]) ->
    [{Id, menu(Tree)} | menu(Rest)];
menu([Id | Rest]) when is_integer(Id); is_atom(Id); is_binary(Id) ->
    [{Id, []} | menu(Rest)];
menu(_) -> [].
