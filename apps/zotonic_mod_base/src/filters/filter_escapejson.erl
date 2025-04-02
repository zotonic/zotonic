%% @author François Cardinaux
%% @copyright 2011-2025 François Cardinaux
%% @doc Zotonic filter to escape JSON strings as specified on http://www.json.org/.
%% Inspired by Marc Worrell's filter_escapejs module
%% @end

%% Copyright 2011-2025 François Cardinaux
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

-module(filter_escapejson).
-export([escapejson/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Escape a value to be used directlty in a string for JSON text output.
-spec escapejson(Value, Context) -> binary() when
    Value :: term(),
    Context :: z:context().
escapejson(undefined, _Context) ->
    <<>>;
escapejson(List, Context) when is_list(List) ->
    {B, _Context1} = z_render:render_html(List, Context),
    escapejson(B, Context);
escapejson(Atom, Context) when is_atom(Atom) ->
    escapejson(atom_to_binary(Atom, utf8), Context);
escapejson(Input, _Context) when is_binary(Input) ->
    z_json:json_escape(Input);
escapejson(#trans{} = Tr, Context) ->
    Text = z_trans:lookup_fallback(Tr, Context),
    escapejson(Text, Context);
escapejson({{Y, M, D}, {H, I, S}}, _Context) when
    is_integer(Y), is_integer(M), is_integer(D),
    is_integer(H), is_integer(I), is_integer(S) ->
    iolist_to_binary(
        io_lib:format(
            "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
            [Y, M, D, H, I, S]
        )
    );
escapejson(Input, _Context) ->
    z_json:json_escape(z_json:encode(Input)).
