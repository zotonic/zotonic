%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc Pretty print filter to dump values in a readable format.

%% Copyright 2010 Marc Worrell
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

-module(filter_pprint).
-export([pprint/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

pprint(#m{} = M, Context) ->
    pprint(z_template_compiler_runtime:to_simple_value(M, Context), Context);
pprint(V, _Context) ->
	z_html:nl2br(z_html:escape(io_lib:format("~p", [V]))).
