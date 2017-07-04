%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Zotonic core - main routines to 'reason' about the current Zotonic
%%      installation.

%% Copyright 2017 Marc Worrell
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

-module(zotonic_core).

-author('Marc Worrell <marc@worrell.nl>').

-export([
    is_zotonic_project/0,
    is_testsandbox/0,
    is_app_available/1
    ]).

%% @doc Check if the current site is running the testsandbox
-spec is_testsandbox() -> boolean().
is_testsandbox() ->
    case atom_to_list(node()) of
        "zotonic001_testsandbox@" ++ _ -> true;
        _ -> false
    end.

%% @doc Check if this running Zotonic is the main Git project.
%%      This is used for e.g. the .pot generation.
is_zotonic_project() ->
    is_app_available(zotonic)
    andalso is_app_available(zotonic_core).

is_app_available(App) ->
    case code:which(App) of
        non_existing -> false;
        Path when is_list(Path) -> true
    end.

