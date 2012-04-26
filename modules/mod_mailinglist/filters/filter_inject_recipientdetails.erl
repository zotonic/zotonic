%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2012-04-26

%% @doc Add details of the mailinglist recipient to the URL.

%% Copyright 2012 Arjan Scherpenisse
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

-module(filter_inject_recipientdetails).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([inject_recipientdetails/3]).

-include("zotonic.hrl").


inject_recipientdetails(Body, undefined, _Context) ->
    Body;
inject_recipientdetails(Body, Recipient, _Context) ->
    Val = case proplists:get_value(props, Recipient, []) of
              Props when is_list(Props) ->
                  [{email, proplists:get_value(email, Recipient)},
                   {name_first, proplists:get_value(name_first, Props)},
                   {name_surname, proplists:get_value(name_surname, Props)},
                   {name_surname_prefix, proplists:get_value(name_surname_prefix, Props)}];
              _ ->
                  [{email, proplists:get_value(email, Recipient)}]
          end,
    Val1 = lists:filter(fun({_, V}) -> not(z_utils:is_empty(V)) end, Val),
    Part = [$?, [ [z_utils:url_encode(z_convert:to_list(K)), $=, z_utils:url_encode(z_convert:to_list(V)), $\\, $&] || {K, V} <- Val1] ],
    Part1 = lists:flatten(Part),
    Part2 = lists:reverse(tl(lists:reverse(Part1))),
    iolist_to_binary(re:replace(Body, "##\"", Part2 ++ "\"", [global])).
