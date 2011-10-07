%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2009-10-02
%% @doc Atom support.

%% Copyright 2009 Arjan Scherpenisse
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

-module(mod_atom).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-mod_title("Atom module").
-mod_description("Provides atom (RFC 4287) representations for resources.").
-mod_prio(1000).

%% interface functions
-export([
    observe_content_types_dispatch/3
]).

-include_lib("zotonic.hrl").

%% Dispatch to the atom representation.
observe_content_types_dispatch(#content_types_dispatch{}, Acc, _Context) ->
    [{"application/atom+xml", atom_entry} | Acc].
