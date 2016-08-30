%% @author Arjan Scherpenisse
%% @copyright 2013 Arjan Scherpenisse

%% @doc Service call implementing test 1: JSON serialization

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

-module(service_development_techempower_json).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-svc_title("Test 1: JSON serialization.").
-svc_needauth(false).

-export([process_get/1]).

process_get(_Context) ->
    {struct, [{message, <<"Hello, World!">>}]}.
