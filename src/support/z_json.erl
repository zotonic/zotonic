%% @author Marc Worrell
%% @copyright 2012 Marc Worrell
%% @doc JSON support routines.

%% Copyright 2012 Marc Worrell
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

-module(z_json).
-include("zotonic.hrl").

-export ([
    json_escape/1,
    to_mochijson/2,
    from_mochijson/2
]).

%%% ESCAPE JSON %%%

%% @doc JSON escape for safe quoting of JSON strings. Subtly different
%% from JS escape, see http://www.json.org/
json_escape(undefined) -> [];
json_escape([]) -> [];
json_escape(<<>>) -> [];
json_escape(Value) when is_integer(Value) -> integer_to_list(Value);
json_escape(Value) when is_atom(Value) -> json_escape(atom_to_list(Value), []);
json_escape(Value) when is_binary(Value) -> json_escape(binary_to_list(Value), []);
json_escape(Value) -> json_escape(Value, []).

json_escape([], Acc) -> lists:reverse(Acc);
json_escape([$" |T], Acc) -> json_escape(T, [$" ,$\\|Acc]);
json_escape([$\\|T], Acc) -> json_escape(T, [$\\,$\\|Acc]);
json_escape([$/ |T], Acc) -> json_escape(T, [$/ ,$\\|Acc]);
json_escape([$\b|T], Acc) -> json_escape(T, [$b ,$\\|Acc]);
json_escape([$\f|T], Acc) -> json_escape(T, [$f ,$\\|Acc]);
json_escape([$\n|T], Acc) -> json_escape(T, [$n ,$\\|Acc]);
json_escape([$\r|T], Acc) -> json_escape(T, [$r ,$\\|Acc]);
json_escape([$\t|T], Acc) -> json_escape(T, [$t ,$\\|Acc]);
json_escape([H|T], Acc) when is_integer(H) ->
    json_escape(T, [H|Acc]);
json_escape([H|T], Acc) ->
    H1 = json_escape(H),
    json_escape(T, [H1|Acc]).



%% @doc Convert a (nested) property to mochijson:encode/1 format.
%%      Select the language in the context for {trans, _} values.
to_mochijson([{_,_}|_] = PS, Context) -> {struct, [ {K, to_mochijson(V, Context)} || {K,V} <- PS ]};
to_mochijson(L, Context) when is_list(L) -> {array, [ to_mochijson(V, Context) || V <- L]};
to_mochijson({trans, _} = Tr, Context) -> z_trans:lookup_fallback(Tr, Context);
to_mochijson({{_,_,_},{_,_,_}} = Date, Context) -> erlydtl_dateformat:format(Date, "c", Context);
to_mochijson(B, _Context) when is_binary(B) -> B;
to_mochijson(B, _Context) when is_number(B) -> B;
to_mochijson(undefined, _Context) -> null;
to_mochijson(B, _Context) when is_atom(B) -> B;
to_mochijson(_, _Context) -> unmappable.


%% @doc Convert a (nested) JSON document to nested property lists
%%      Properties are not converted to 
%% @todo Translate 
from_mochijson({struct, Props}, Context) -> [ opt_convert({K,from_mochijson(V, Context)}) || {K,V} <- Props ];
from_mochijson({array, Values}, Context) -> [ from_mochijson(V, Context) || V <- Values ];
from_mochijson(N, _Context) when is_number(N) -> N;
from_mochijson(null, _Context) -> undefined;
from_mochijson(Atom, _Context) when is_atom(Atom) -> Atom;
from_mochijson(V, _Context) -> V.

    opt_convert(X) -> X.
