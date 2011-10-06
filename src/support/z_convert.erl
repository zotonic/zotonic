%% @author Rusty Klophaus
%% @copyright Copyright (c) 2008-2009 Rusty Klophaus, Copyright (c) 2009 Marc Worrell
%%
%% @doc Conversion functions for all kinds of data types. Changes to
%% Rusty's version: added date conversion, undefined handling and more
%% to_bool cases.

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
	to_flatlist/1,
	to_atom/1, 
	to_binary/1, 
	to_integer/1,
	to_float/1,
	to_bool_strict/1,
	to_bool/1,
	to_utc/1,
	to_localtime/1,
	to_datetime/1,
    to_date/1,
    to_time/1,
    to_isotime/1,
    to_json/1,

	convert_json/1,
    ip_to_list/1
]).


-include("zotonic.hrl").


%%% CONVERSION %%%

clean_lower(L) -> string:strip(z_string:to_lower(to_list(L))).

to_list(undefined) -> [];
to_list(<<>>) -> [];
to_list({rsc_list, L}) -> L;
to_list(L) when is_list(L) -> lists:flatten(L);
to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(F) when is_float(F) -> float_to_list(F).

to_flatlist(L) when is_list(L) ->
	case z_string:is_string(L) of
		true -> L;
		false -> to_list(iolist_to_binary(L))
	end;
to_flatlist(L) ->
	to_list(L).


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
to_binary(F) when is_float(F) -> to_binary(float_to_list(F));
to_binary(L) when is_list(L) -> list_to_binary(L).

to_integer(undefined) -> undefined;
to_integer([]) -> undefined;
to_integer(A) when is_atom(A) -> to_integer(atom_to_list(A));
to_integer(B) when is_binary(B) -> to_integer(binary_to_list(B));
to_integer(I) when is_integer(I) -> I;
to_integer(F) when is_float(F) -> erlang:round(F);
to_integer([C]) when is_integer(C) andalso (C > $9 orelse C < $0) -> C;
to_integer(L) when is_list(L) -> list_to_integer(L).

to_float(undefined) -> undefined;
to_float([]) -> undefined;
to_float(A) when is_atom(A) -> to_float(atom_to_list(A));
to_float(B) when is_binary(B) -> to_float(binary_to_list(B));
to_float(I) when is_integer(I) -> I + 0.0;
to_float(F) when is_float(F) -> F;
to_float(L) when is_list(L) -> 
    case lists:member($., L) of
        true -> list_to_float(L);
        false -> list_to_float(L++".0")  %% list_to_float("1") gives a badarg
    end.


%% @doc Quite loose conversion of values to boolean
to_bool("false") -> false;
to_bool("FALSE") -> false;
to_bool("n") -> false;
to_bool("N") -> false;
to_bool("NO") -> false;
to_bool(<<"false">>) -> false;
to_bool(<<"FALSE">>) -> false;
to_bool(<<"n">>) -> false;
to_bool(<<"N">>) -> false;
to_bool(<<"NO">>) -> false;
to_bool("disabled") -> false;
to_bool(<<"disabled">>) -> false;
to_bool("DISABLED") -> false;
to_bool(<<"DISABLED">>) -> false;
to_bool([0]) -> false;
to_bool(V) -> to_bool_strict(V).

% @doc Convert values to boolean values according to the Django rules
to_bool_strict(undefined) -> false;
to_bool_strict(false) -> false;
to_bool_strict(0) -> false;
to_bool_strict(0.0) -> false;
to_bool_strict(<<>>) -> false;
to_bool_strict(<<0>>) -> false;
to_bool_strict([]) -> false;
to_bool_strict({rsc_list, []}) -> false;
to_bool_strict(#m{value=V}) -> to_bool(V);
to_bool_strict(#m_search_result{result=V}) -> to_bool(V);
to_bool_strict(#search_result{result=[]}) -> false;
to_bool_strict("0") -> false;
to_bool_strict(<<"0">>) -> false;
to_bool_strict(_) -> true.


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

%% @doc Convert an input to a datetime, using to_date/1 and to_time/1.
to_datetime({{_,_,_},{_,_,_}} = DT) -> DT;
to_datetime({_,_,_} = D) -> {D, {0,0,0}};
to_datetime(B) when is_binary(B) ->
    to_datetime(binary_to_list(B));
to_datetime(L) when is_list(L) ->
	try
		case string:tokens(L, " T") of
			[Date,Time] ->
                WithTZ = fun(Tm, Tz, Mul) ->
                                 TZTime = to_time(Tz),
                                 Add = calendar:datetime_to_gregorian_seconds({{0,1,1},TZTime}),
                                 Secs = calendar:datetime_to_gregorian_seconds({to_date(Date), to_time(Tm)}),
                                 calendar:universal_time_to_local_time(calendar:gregorian_seconds_to_datetime(Secs+(Mul*Add)))
                         end,
                case string:tokens(Time, "+") of
                    [Time1, TZ] ->
                        %% Timestamp with positive time zone
                        WithTZ(Time1, TZ, -1);
                    _ ->
                        case string:tokens(Time, "-") of
                            [Time1, TZ] ->
                                %% Timestamp with negative time zone
                                WithTZ(Time1, TZ, 1);
                            _ ->
                                case lists:reverse(Time) of
                                    [$Z|Rest] ->
                                        %% Timestamp ending on Z (= UTC)
                                        calendar:universal_time_to_local_time({to_date(Date), to_time(lists:reverse(Rest))});
                                    _ ->
                                        %% Timestamp without time zone
                                        {to_date(Date), to_time(Time)}
                                end
                        end
                end;
			[Date] ->
                {to_date(Date), {0,0,0}}
        end
	catch
		_:_ -> undefined
	end;
to_datetime(undefined) ->
	undefined.

%% @doc Convert an input to a date.
to_date({_,_,_} = D) -> D;
to_date(B) when is_binary(B) ->
    to_date(binary_to_list(B));
to_date([]) -> undefined;
to_date(L) when is_list(L) ->
    case string:tokens(L, "-/") of
        [D,M,Y] when length(Y) =:= 4 ->
            {to_integer(Y),to_integer(M),to_integer(D)};
        [Y,M,D] ->
            {to_integer(Y),to_integer(M),to_integer(D)}
    end.

%% @doc Convert an input to a time.
to_time({_,_,_} = D) -> D;
to_time(B) when is_binary(B) ->
    to_time(binary_to_list(B));
to_time([]) -> undefined;
to_time([H1,H2,M1,M2]) ->
    to_time([H1,H2,$:,M1,M2]);
to_time(L) when is_list(L) ->
    [H,I,S|_] = lists:flatten([[to_integer(X) ||X <-  string:tokens(L, ":.")], 0, 0]),
    {H,I,S}.

%% @doc Convert a datetime (in local time) to an ISO time string (in universal time).
%% @spec to_isotime(DateTime) -> string()
to_isotime(DateTime) ->
    z_convert:to_list(erlydtl_dateformat:format(hd(calendar:local_time_to_universal_time_dst(DateTime)), "Y-m-d\\TH:i:s\\Z", #context{})).


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
    erlydtl_dateformat:format(DateTime, "Y-m-d H:i:s", en);
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
    


ip_to_list({IP,Port}) when is_tuple(IP), is_integer(Port) ->
    ip_to_list(IP);
ip_to_list({N1,N2,N3,N4} ) ->
    lists:flatten([integer_to_list(N1), $., integer_to_list(N2), $., integer_to_list(N3), $., integer_to_list(N4)]);
ip_to_list({_K1,_K2,_K3,_K4,_K5,_K6,_K7,_K8} = IPv6) ->
    L = lists:map(fun(0) -> "";
                     (N) -> io_lib:format("~.16b", [N])
                  end,
                  tuple_to_list(IPv6)),
    lists:flatten(string:join(L, ":")).


%% @doc Convert json from facebook favour to an easy to use format for zotonic templates.
convert_json({K, V}) when is_binary(K) ->
    {z_convert:to_atom(K), convert_json(V)};
convert_json({struct, PropList}) when is_list(PropList) ->
    convert_json(PropList);
convert_json(L) when is_list(L) ->
    [convert_json(V) || V <- L];
convert_json(V) ->
    V.
