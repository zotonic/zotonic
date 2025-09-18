%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2015 Marc Worrell
%% Date: 2015-10-22
%% @doc Simple resource that just returns "Hello, World!".

%% Copyright 2015 Maas-Maarten Zeeman
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

-module(controller_hello_world).
-moduledoc("
See also

[controller\\_ping](/id/doc_controller_controller_ping)

Simple controller that always serves the string `Hello, World!`

This controller can be used for a dispatch rule to check if the server is responding.
").
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl").

-export([
    process/4
]).

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    {<<"Hello, World!">>, Context}.
