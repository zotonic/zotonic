%% @author Rusty Klophaus
%% @copyright Copyright (c) 2008-2009 Rusty Klophaus
%% @copyright Copyright (c) 2009 Marc Worrell
%%
%% @doc Conversion functions for all kinds of data types. 
%% @doc Changes to Rusty's version: added date conversion, undefined handling and more to_bool cases.

%% Copyright 2009 Marc Worrell
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

-module(z_convert).
-author("Rusty Klophaus").
-author("Marc Worrell <marc@worrell.nl>").
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").


-export ([
	clean_lower/1,
	to_list/1, 
	to_atom/1, 
	to_binary/1, 
	to_integer/1,
	to_bool/1,
	to_utc/1,
	to_localtime/1,
    to_json/1
]).


-include("zotonic.hrl").


%%% CONVERSION %%%

clean_lower(L) -> string:strip(string:to_lower(to_list(L))).

to_list(undefined) -> [];
to_list(<<>>) -> [];
to_list(L) when is_list(L) -> inner_to_list(lists:flatten(L));
to_list(A) -> inner_to_list(A).

inner_to_list(A) when is_atom(A) -> atom_to_list(A);
inner_to_list(B) when is_binary(B) -> binary_to_list(B);
inner_to_list(I) when is_integer(I) -> integer_to_list(I);
inner_to_list(L) when is_list(L) -> L.

to_atom(<<>>) -> undefined;
to_atom([]) -> undefined;
to_atom(A) when is_atom(A) -> A;
to_atom(B) when is_binary(B) -> to_atom(binary_to_list(B));
to_atom(I) when is_integer(I) -> to_atom(integer_to_list(I));
to_atom(L) when is_list(L) -> list_to_atom(binary_to_list(list_to_binary(L))).

to_binary(undefined) -> <<>>;
to_binary(A) when is_atom(A) -> to_binary(atom_to_list(A));
to_binary(B) when is_binary(B) -> B;
to_binary(I) when is_integer(I) -> to_binary(integer_to_list(I));
to_binary(L) when is_list(L) -> list_to_binary(L).

to_integer(undefined) -> undefined;
to_integer([]) -> undefined;
to_integer(A) when is_atom(A) -> to_integer(atom_to_list(A));
to_integer(B) when is_binary(B) -> to_integer(binary_to_list(B));
to_integer(I) when is_integer(I) -> I;
to_integer(L) when is_list(L) -> list_to_integer(L).

to_bool(undefined) -> false;
to_bool(false) -> false;
to_bool(0) -> false;
to_bool(0.0) -> false;
to_bool(<<>>) -> false;
to_bool(<<0>>) -> false;
to_bool(<<"0">>) -> false;
to_bool([]) -> false;
to_bool("0") -> false;
to_bool([0]) -> false;
to_bool("false") -> false;
to_bool("FALSE") -> false;
to_bool("n") -> false;
to_bool("N") -> false;
to_bool(<<"false">>) -> false;
to_bool(<<"FALSE">>) -> false;
to_bool(<<"n">>) -> false;
to_bool(<<"N">>) -> false;
to_bool("disabled") -> false;
to_bool(<<"disabled">>) -> false;
to_bool("DISABLED") -> false;
to_bool(<<"DISABLED">>) -> false;
to_bool(_) -> true.


%% @doc Convert a local date time to utc
to_utc(undefined) ->
    undefined;
to_utc({{9999,_,_}, _}) ->
    ?ST_JUTTEMIS;
to_utc(D) ->
    case catch calendar:local_time_to_universal_time_dst(D) of
        [] -> D;    % This time never existed in the local time, just take it as-is
        [UTC] -> UTC;
        [DstUTC, _UTC] -> DstUTC;
        {'EXIT', _} -> D
    end.


%% @doc Convert a utc date time to local
to_localtime(undefined) ->
    undefined;
to_localtime({{9999,_,_},_}) ->
    ?ST_JUTTEMIS;
to_localtime(D) ->
    case catch calendar:universal_time_to_local_time(D) of
        {'EXIT', _} -> D;
        LocalD -> LocalD
    end.

%%
%% @doc Convert an Erlang structure to a format that can be serialized by mochijson.
%%

%% Simple values
to_json(undefined) ->
    null;
to_json(X) when is_atom(X) ->
    X;
to_json(X) when is_integer(X) ->
    X;
to_json(X) when is_binary(X) ->
    X;

%% Tuple values
to_json({{Y,M,D},{H,I,S}} = DateTime)
    when is_integer(Y), is_integer(M), is_integer(D),
         is_integer(H), is_integer(I), is_integer(S) ->
    erlydtl_dateformat:format(DateTime, "Y-m-d H:i:s");
to_json({array, X}) ->
    %% Explicit request for array (to prevent string conversion for some lists)
    {array, [to_json(V) || V <- X]};
to_json({X, Y}) ->
    {struct, to_json_struct([{X, Y}])};
to_json(X) when is_tuple(X) ->
    {array, [to_json(V) || V <- tuple_to_list(X)]};

%% List values
to_json([{X, Y}]) when is_atom(X) ->
    {struct, to_json_struct([{X, Y}])};
to_json([{X, Y} | Z]) when is_atom(X) ->
    {struct, to_json_struct([{X, Y} | Z])};
to_json(X) when is_list(X) ->
    case z_string:is_string(X) of
        true ->
            X;
        false ->
            {array, [to_json(V) || V <- X]}
    end.

%% Handle structs specially
to_json_struct([]) ->
    [];
to_json_struct([{X,Y}|T]) ->
    [{to_json_struct_key(X), to_json(Y)} | to_json_struct(T)].
to_json_struct_key(X) when is_atom(X) orelse is_integer(X) orelse is_binary(X) ->
    X;
to_json_struct_key(X) when is_list(X) ->
    case z_string:is_string(X) of
        true ->
            X;
        false ->
            invalid_key
    end;
to_json_struct_key(_) ->
    invalid_key.
    
    
