%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell
%% @doc Encode a list of values using CSV

%% Copyright 2013 Marc Worrell
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

-module(export_encode_csv).
-author("Marc Worrell <marc@worrell.nl>").

-export([
	encode/2
	]).


encode([], _Context) ->
	<<"\r\n">>;
encode([V], Context) ->
	iolist_to_binary([
		encode_value(V, Context),
		<<"\r\n">>
		]);
encode([V|Xs], Context) ->
	iolist_to_binary([
		encode_value(V, Context),
		[ [$,, encode_value(X, Context)] || X <- Xs ],
		<<"\r\n">>
		]).

encode_value(undefined, _Context) ->
	<<>>;
encode_value(<<>>, _Context) ->
	<<>>;
encode_value(N, _Context) when is_integer(N) ->
	z_convert:to_binary(N);
encode_value(N, _Context) when is_float(N) ->
	z_convert:to_binary(N);
encode_value(B, _Context) when is_binary(B) ->
	quote(escape(B));
encode_value({Y,M,D} = Date, Context)
	when is_integer(Y), is_integer(M), is_integer(D) ->
	quote(erlydtl_dateformat:format({Date, {0,0,0}}, "Y-m-d", Context));
encode_value({{Y,M,D}, {H,I,S}} = Date, Context) 
	when is_integer(Y), is_integer(M), is_integer(D),
		 is_integer(H), is_integer(I), is_integer(S) ->
	quote(erlydtl_dateformat:format(Date, "Y-m-d H:i:s", Context));
encode_value({trans, _} = Trans, Context) ->
	encode_value(z_trans:lookup_fallback(Trans, Context), Context);
encode_value(N, Context) ->
	encode_value(z_convert:to_binary(N), Context).

quote(B) -> [$", B, $"].

% We need to recognize fields starting with a '=', as Excel thinks that is a formula.
escape(<<$=, B/binary>>) -> escape(B, <<" =">>);
escape(B) -> escape(B, <<>>).

escape(<<>>, Acc) -> Acc;
escape(<<$", B/binary>>, Acc) -> escape(B, <<Acc/binary, $", $">>);
escape(<<10, B/binary>>, Acc) -> escape(B, <<Acc/binary, 32>>);
escape(<<13, 10, B/binary>>, Acc) -> escape(B, <<Acc/binary, 32>>);
escape(<<13, B/binary>>, Acc) -> escape(B, <<Acc/binary, 32>>);
escape(<<X, B/binary>>, Acc) when X < 32 -> escape(B, Acc);
escape(<<X, B/binary>>, Acc) -> escape(B, <<Acc/binary, X>>).



