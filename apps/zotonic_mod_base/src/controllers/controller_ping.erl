%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Simple controller that just responds with 'pong'. Used for connection tests.

%% Copyright 2021 Marc Worrell
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

-module(controller_ping).
-moduledoc("
Simple controller for connection tests, used on the `/test/connection` page.

It always responds with the four character string `pong`. The ususal path is: `/.zotonic/ping`
").
-author("Marc Worrell <marc@worrell.nl").

-export([
    content_types_provided/1,
    process/4
]).


content_types_provided(Context) ->
    {[ {<<"text">>, <<"plain">>, []} ], Context}.

process(_, _, _, Context) ->
    Context1 = z_context:set_nocache_headers(Context),
    {<<"pong">>, Context1}.
