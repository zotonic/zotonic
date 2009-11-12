%% @author Marc Worrell
%% @copyright 2009 Marc Worrell
%% 
%% Parts are from wf_utils.erl which is Copyright (c) 2008-2009 Rusty Klophaus
%%
%% @doc Misc utility functions for zotonic

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

-module(z_utils).
-include("zotonic.hrl").

-export ([
    now/0,
	get_seconds/0,
	f/1,
	f/2,
	encode/2,
	decode/2,
	hex_encode/1,
	hex_decode/1,
	checksum/2,
	checksum_assert/3,
	pickle/2,
	depickle/2,
	url_encode/1,
	os_escape/1,
	js_escape/1,
	js_object/1,
	js_object/2,
	combine/2,
	combine_defined/2,
	only_letters/1,
	only_digits/1,
	prefix/2,
	coalesce/1,
	is_process_alive/1,
	is_true/1,
	assert/2,
	prop_replace/3,
	prop_delete/2,
	group_proplists/2,
	index_proplist/2,
	randomize/1,
	randomize/2,
	split/2,
	split_in/2,
	vsplit_in/2,
	group_by/3,
	replace1/3,
	list_dir_recursive/1,
	are_equal/2,
	name_for_host/2
]).

%%% FORMAT %%%

f(S) -> f(S, []).
f(S, Args) -> lists:flatten(io_lib:format(S, Args)).


%% @doc Return the current tick count
now() ->
    {M,S,_M} = erlang:now(),
    M*1000 + S.


%% @doc Return the current universal time in seconds
get_seconds() -> calendar:datetime_to_gregorian_seconds(calendar:universal_time()).


%% @doc Multinode is_process_alive check
is_process_alive(Pid) ->
	case is_pid(Pid) of
		true -> 
			% If node(Pid) is down, rpc:call returns something other than
			% true or false.
			case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
				true -> true;
				_ -> false
			end;
		_ -> false
	end.

%%% HEX ENCODE and HEX DECODE

hex_encode(Data) -> encode(Data, 16).
hex_decode(Data) -> decode(Data, 16).

encode(Data, Base) when is_binary(Data) -> encode(binary_to_list(Data), Base);
encode(Data, Base) when is_list(Data) ->
	F = fun(C) when is_integer(C) ->
		case erlang:integer_to_list(C, Base) of
			[C1, C2] -> [C1, C2];
			[C1]     -> [$0, C1]
		end
	end,
	[F(I) || I <- Data].
	
decode(Data, Base) when is_binary(Data) -> decode(binary_to_list(Data), Base);
decode(Data, Base) when is_list(Data) -> 	
	inner_decode(Data, Base).

inner_decode(Data, Base) when is_list(Data) ->
	case Data of
		[C1, C2|Rest] -> 
			I = erlang:list_to_integer([C1, C2], Base),
			[I|inner_decode(Rest, Base)];
		[] -> 
			[]
	end.


%%% CHECKSUM %%%

checksum(Data, Context) ->
    Sign = z_ids:sign_key_simple(Context),
    z_utils:hex_encode(erlang:md5([Sign,Data])).

checksum_assert(Data, Checksum, Context) ->
    Sign = z_ids:sign_key_simple(Context),
    assert(list_to_binary(z_utils:hex_decode(Checksum)) == erlang:md5([Sign,Data]), checksum_invalid).

%%% PICKLE / UNPICKLE %%%

pickle(Data, Context) ->
    BData = erlang:term_to_binary(Data),
	Nonce = z_ids:number(1 bsl 31),
	Sign  = z_ids:sign_key(Context),
	SData = <<BData/binary, Nonce:32, Sign/binary>>,
	<<C1:64,C2:64>> = erlang:md5(SData),
	base64:encode(<<C1:64, C2:64, Nonce:32, BData/binary>>).
	
depickle(Data, Context) ->
    try
        <<C1:64, C2:64, Nonce:32, BData/binary>> = base64:decode(Data),
    	Sign  = z_ids:sign_key(Context),
    	SData = <<BData/binary, Nonce:32, Sign/binary>>,
    	<<C1:64, C2:64>> = erlang:md5(SData),
    	erlang:binary_to_term(BData)
    catch
        _M:_E -> erlang:throw("Postback data invalid, could not depickle: "++Data)
    end.
	
%%% URL ENCODE %%%

url_encode(S) -> quote_plus(S).

% quote_plus and hexdigit are from Mochiweb.

-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
                      C =:= $_))).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

quote_plus(Atom) when is_atom(Atom) ->
    quote_plus(atom_to_list(Atom));
quote_plus(Int) when is_integer(Int) ->
    quote_plus(integer_to_list(Int));
quote_plus(String) ->
    quote_plus(String, []).

quote_plus([], Acc) ->
    lists:reverse(Acc);
quote_plus([C | Rest], Acc) when ?QS_SAFE(C) ->
    quote_plus(Rest, [C | Acc]);
quote_plus([$\s | Rest], Acc) ->
    quote_plus(Rest, [$+ | Acc]);
quote_plus([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    quote_plus(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]).

%% @spec os_escape(String) -> String
%% @doc Simple escape function for command line arguments
os_escape(A) when is_binary(A) ->
    os_escape(binary_to_list(A));
os_escape(A) when is_list(A) -> 
    os_escape(lists:flatten(A), []).

os_escape([], Acc) ->
    lists:reverse(Acc);
os_escape([C|Rest], Acc) when 
                (C >= $A andalso C =< $Z)
         orelse (C >= $a andalso C =< $z)
         orelse (C >= $0 andalso C =< $9)
         orelse C == $_
         orelse C == $.
         orelse C == $-
         orelse C == $+
         orelse C == $/
         orelse C == $(
         orelse C == $)
         orelse C == 32
    -> 
    os_escape(Rest, [C|Acc]);
os_escape([C|Rest], Acc) when 
                C >= 32
        orelse  C == $\r
        orelse  C == $\n
        orelse  C == $\t
    ->
    os_escape(Rest, [C,$\\|Acc]).


%%% ESCAPE JAVASCRIPT %%%

%% @doc Javascript escape, see also: http://code.google.com/p/doctype/wiki/ArticleXSSInJavaScript

js_escape(undefined) -> [];
js_escape([]) -> [];
js_escape(<<>>) -> [];
js_escape(Value) when is_atom(Value) ->  js_escape(atom_to_list(Value), []);
js_escape(Value) when is_binary(Value) -> js_escape(binary_to_list(Value), []);
js_escape(Value) -> js_escape(Value, []).

js_escape([], Acc) -> lists:reverse(Acc);
js_escape([$\\|T], Acc) -> js_escape(T, [$\\,$\\|Acc]);
js_escape([$\n|T], Acc) -> js_escape(T, [$n,$\\|Acc]);
js_escape([$\r|T], Acc) -> js_escape(T, [$r,$\\|Acc]);
js_escape([$\t|T], Acc) -> js_escape(T, [$t,$\\|Acc]);
js_escape([$'|T], Acc) -> js_escape(T, [$7,$2,$x,$\\|Acc]);
js_escape([$"|T], Acc) -> js_escape(T, [$7,$2,$x,$\\|Acc]);
js_escape([$<|T], Acc) -> js_escape(T, [$c,$3,$x,$\\|Acc]);
js_escape([$>|T], Acc) -> js_escape(T, [$e,$3,$x,$\\|Acc]);
js_escape([$=|T], Acc) -> js_escape(T, [$d,$3,$x,$\\|Acc]);
js_escape([$&|T], Acc) -> js_escape(T, [$6,$2,$x,$\\|Acc]);
%% js_escape([16#85,C|T], Acc) when C >= 16#80 -> js_escape(T, [C,16#85|Acc]);
%% js_escape([16#85|T], Acc) -> js_escape(T, [$5,$8,$0,$0,$u,$\\|Acc]);
js_escape([16#2028|T],Acc)-> js_escape(T, [$8,$2,$0,$2,$u,$\\|Acc]);
js_escape([16#2029|T],Acc)-> js_escape(T, [$9,$2,$0,$2,$u,$\\|Acc]);
js_escape([16#e2,16#80,16#a8|T],Acc)-> js_escape(T, [$8,$2,$0,$2,$u,$\\|Acc]);
js_escape([16#e2,16#80,16#a9|T],Acc)-> js_escape(T, [$9,$2,$0,$2,$u,$\\|Acc]);
js_escape([H|T], Acc) when is_integer(H) -> 
    js_escape(T, [H|Acc]);
js_escape([H|T], Acc) -> 
    H1 = js_escape(H),
    js_escape(T, [H1|Acc]).

%% js_escape(<<"<script", Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, "<scr\" + \"ipt">>);
%% js_escape(<<"script>", Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, "scr\" + \"ipt>">>);


%% @doc Create a javascript object from a proplist
js_object([]) -> <<"{}">>;
js_object(L) -> js_object(L,[]).

js_object(L, []) -> js_object1(L, []);
js_object(L, [Key|T]) -> js_object(proplists:delete(Key,L), T).

%% recursively add all properties as object properties
js_object1([], Acc) -> 
    [${, combine($,,Acc), $}];
js_object1([{Key,Value}|T], Acc) ->
    Prop = [atom_to_list(Key), $:, js_prop_value(Key, Value)],
    js_object1(T, [Prop|Acc]).

js_prop_value(_, undefined) -> <<"null">>;
js_prop_value(_, true) -> <<"true">>;
js_prop_value(_, false) -> <<"true">>;
js_prop_value(_, Atom) when is_atom(Atom) -> [$",js_escape(erlang:atom_to_list(Atom)), $"];
js_prop_value(pattern, [$/|T]=List) ->
    %% Check for regexp
    case length(T) of
        Len when Len =< 2 -> 
            [$",js_escape(List),$"];
        _Len ->
            case string:rchr(T, $/) of
                0 -> 
                    [$",js_escape(List),$"];
                N -> 
                    {_Re, [$/|Options]} = lists:split(N-1,T),
                    case only_letters(Options) of
                        true -> List;
                        false -> [$",js_escape(List),$"]
                    end
            end
    end;
js_prop_value(_, Int) when is_integer(Int) -> io_lib:write(Int);
js_prop_value(_, Value) -> [$",js_escape(Value),$"].


only_letters([]) -> 
    true;
only_letters([C|T]) when (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) ->
    only_letters(T);
only_letters(_) ->
    false.

only_digits([]) ->
    false;
only_digits(L) when is_list(L) ->
    only_digits1(L);
only_digits(B) when is_binary(B) ->
    only_digits(binary_to_list(B)).

    only_digits1([]) -> 
        true;
    only_digits1([C|R]) when C >= $0 andalso C =< $9 ->
        only_digits1(R);
    only_digits1(_) ->
        false.


combine_defined(Sep, List) ->
    List2 = lists:filter(fun(X) -> X /= undefined end, List),
    combine(Sep, List2).

combine(_Sep, []) -> [];
combine(_Sep, [A]) -> [A];
combine(Sep, [H|T]) -> [H, prefix(Sep, T)].

prefix(Sep, List) -> prefix(Sep,List,[]).

prefix(_Sep, [], Acc) -> lists:reverse(Acc);
prefix(Sep, [H|T], Acc) -> prefix(Sep, T, [H,Sep|Acc]).


%%% COALESCE %%%

coalesce([]) -> undefined;
coalesce([H]) -> H;
coalesce([undefined|T]) -> coalesce(T);
coalesce([[]|T]) -> coalesce(T);
coalesce([H|_]) -> H.


%% @doc Check if the parameter could represent the logical value of "true"    
is_true([$t|_T]) -> true;
is_true([$y|_T]) -> true;
is_true("on") -> true;
is_true("1") -> true;

is_true(<<"true">>) -> true;
is_true(<<"yes">>) -> true;
is_true(<<"on">>) -> true;
is_true(<<"1">>) -> true;

is_true(true) -> true;
is_true(yes) -> true;
is_true(on) -> true;

is_true(N) when is_integer(N) andalso N /= 0 -> true;

is_true(_) -> false.


%% @spec assert(bool(), error) -> none()
%% @doc Check if an assertion is ok or failed
assert(false, Error) -> erlang:error(Error);
assert(_, _) -> ok.


%% @doc Replace a property in a proplist
prop_replace(Prop, Value, List) ->
    [{Prop,Value} | lists:keydelete(Prop,1,List)].

prop_delete(Prop, List) ->
    lists:keydelete(Prop, 1, List).

%% @doc Given a list of proplists, make it a nested list with respect to a property, combining elements
%% with the same property.  Assumes the list is sorted on the property you are splitting on
%% For example:  [[{a,b}{x}], [{a,b}{z}], [{a,c}{y}]] gives:
%%   [ {b, [[{a,b}{x}], [{a,b}{z}]]},  {c, [[{a,c}{y}]]} ]
%% @spec group_proplists(Property, [PropList]) -> PropList
group_proplists(_Prop, []) -> 
    [];
group_proplists(Prop, [Item|Rest]) ->
    PropValue = proplists:get_value(Prop, Item),
    group_proplists(Prop, PropValue, Rest, [Item], []).

group_proplists(_Prop, _PropValue, [], [], Result) ->
    lists:reverse(Result);
group_proplists(Prop, PropValue, [], Acc, Result) ->
    lists:reverse(Acc),
    group_proplists(Prop, PropValue, [], [], [{z_convert:to_atom(PropValue),Acc}|Result]);
group_proplists(Prop, PropValue, [C|Rest], Acc, Result) ->
    case proplists:get_value(Prop, C) of
        PropValue -> 
            group_proplists(Prop, PropValue, Rest, [C|Acc], Result);
        Other ->
            group_proplists(Prop, Other, Rest, [C], [{z_convert:to_atom(PropValue),Acc}|Result])
    end.


%% @doc Make a property list based on the value of a property
%% For example:  [  [{a,b}], [{a,c}] ]  gives  [{a, [{a,b}]}, {c, [[{a,c}]]}]
%% @spec index_proplist(Property, [PropList]) -> PropList
index_proplist(_Prop, []) ->
    [];
index_proplist(Prop, List) ->
    index_proplist(Prop, List, []).

index_proplist(_Prop, [], Acc) ->
    lists:reverse(Acc);
index_proplist(Prop, [L|Rest], Acc) ->
    index_proplist(Prop, Rest, [{z_convert:to_atom(proplists:get_value(Prop,L)),L}|Acc]).


%% @doc Simple randomize of a list. Not good quality, but good enough for us
randomize(List) ->
    {A1,A2,A3} = erlang:now(),
    random:seed(A1, A2, A3),
    D = lists:map(fun(A) ->
                    {random:uniform(), A}
             end, List),
    {_, D1} = lists:unzip(lists:keysort(1, D)), 
    D1.

randomize(N, List) ->
    split(N, randomize(List)).

split(N, L) ->
    split(N,L,[]).

split(_N, [], Acc) ->
    {lists:reverse(Acc), []};
split(0, Rest, Acc) ->
    {lists:reverse(Acc), Rest};
split(N, [A|Rest], Acc) ->
    split(N-1, Rest, [A|Acc]).


split_in(L, N) when N =< 1 ->
    L;
split_in(L, N) when is_binary(L) ->
    split_in(binary_to_list(L), N);
split_in(L, N) when is_list(L) ->
    [ lists:reverse(SubList) || SubList <- split_in(L, [], split_in_acc0(N, [])) ].

    split_in_acc0(0, Acc) -> Acc;
    split_in_acc0(N, Acc) -> split_in_acc0(N-1, [[] | Acc]).

    split_in([], Acc1, Acc0) ->
        lists:reverse(Acc1) ++ Acc0;
    split_in(L, Acc1, []) ->
        split_in(L, [], lists:reverse(Acc1));
    split_in([H|T], Acc1, [HA|HT]) ->
        split_in(T, [[H|HA]|Acc1], HT).


vsplit_in(L, N) when N =< 1 ->
	L;
vsplit_in(L, N) when is_binary(L) ->
	vsplit_in(binary_to_list(L), N);
vsplit_in(L, N) ->
	Len = length(L),
	RunLength = case Len rem N of
		0 -> Len div N;
		_ -> Len div N + 1
	end,
	vsplit_in(N, L, RunLength, []).
	
	vsplit_in(1, L, _, Acc) ->
		lists:reverse([L|Acc]);
	vsplit_in(N, L, RunLength, Acc) ->
		{Row,Rest} = lists:split(RunLength, L),
		vsplit_in(N-1, Rest, RunLength, [Row|Acc]).

		
%% @doc Group by a property or m_rsc property, keeps the input list in the same order.
group_by([], _, _Context) ->
    [];
group_by(L, Prop, Context) ->
    LP = [ group_by_addprop(H, Prop, Context) || H <- L ],
    Dict1 = group_by_dict(LP, dict:new()),
    group_by_fetch_in_order(LP, [], Dict1, []).

    group_by_fetch_in_order([], _, _, Acc) ->
        lists:reverse(Acc);
    group_by_fetch_in_order([{Key,_}|T], Ks, Dict, Acc) ->
        case lists:member(Key, Ks) of
            true -> group_by_fetch_in_order(T, Ks, Dict, Acc);
            false -> group_by_fetch_in_order(T, [Key|Ks], Dict, [dict:fetch(Key, Dict)|Acc])
        end.
    
    group_by_dict([], Dict) ->
        Dict;
    group_by_dict([{Key,V}|T], Dict) ->
        case dict:is_key(Key, Dict) of
            true -> group_by_dict(T, dict:append(Key, V, Dict));
            false -> group_by_dict(T, dict:store(Key, [V], Dict))
        end.
    
    group_by_addprop(Id, Prop, Context) when is_integer(Id) ->
        {m_rsc:p(Id, Prop, Context), Id};
    group_by_addprop(L, Prop, _Context) when is_list(L) ->
        {proplists:get_value(Prop, L), L};
    group_by_addprop(N, _Prop, _Context) ->
        {undefined, N}.


replace1(F, T, L) ->
    replace1(F, T, L, []).
replace1(_F, _T, [], Acc) ->
    lists:reverse(Acc);
replace1(F, T, [F|R], Acc) ->
    replace1(F, T, R, [T|Acc]);
replace1(F, T, [C|R], Acc) ->
    replace1(F, T, R, [C|Acc]).


%% @doc Return a list of all files in a directory, recursive depth first search for files not starting with a '.'
list_dir_recursive(Dir) ->
    case file:list_dir(Dir) of 
        {ok, Files} ->
            list_dir_recursive(Files, Dir, []);
        {error, _} ->
            []
    end.

    list_dir_recursive([], _BaseDir, Acc) -> 
        Acc;
    list_dir_recursive([[$.|_]|OtherFiles], BaseDir, Acc) ->
        list_dir_recursive(OtherFiles, BaseDir, Acc);
    list_dir_recursive([File|OtherFiles], BaseDir, Acc) ->
        Path = filename:join(BaseDir, File),
        case filelib:is_regular(Path) of
            true ->
                list_dir_recursive(OtherFiles, BaseDir, [File|Acc]);
            false ->
                case filelib:is_dir(Path) of
                    true ->
                        Acc1 = case file:list_dir(Path) of
                            {ok, Files} ->
                                NonDotFiles = lists:filter(fun([$.|_]) -> false; (_) -> true end, Files),
                                RelFiles = [ filename:join(File, F) || F <- NonDotFiles],
                                list_dir_recursive(RelFiles, BaseDir, Acc);
                            {error, _} ->
                                Acc
                        end,
                        list_dir_recursive(OtherFiles, BaseDir, Acc1);
                    false -> 
                        list_dir_recursive(OtherFiles, BaseDir, Acc)
                end
        end.


%% @doc Check if two arguments are equal, optionally converting them
are_equal(Arg1, Arg2) when Arg1 =:= Arg2 ->
    true;
are_equal(Arg1, Arg2) when is_boolean(Arg1) ->
    Arg1 == z_convert:to_bool(Arg2);
are_equal(Arg1, Arg2) when is_boolean(Arg2) ->
    Arg2 == z_convert:to_bool(Arg1);
are_equal(Arg1, Arg2) when is_atom(Arg1) ->
    are_equal(atom_to_list(Arg1), Arg2);
are_equal(Arg1, Arg2) when is_atom(Arg2) ->
    are_equal(Arg1, atom_to_list(Arg2));
are_equal(Arg1, Arg2) when is_binary(Arg1) ->
    are_equal(binary_to_list(Arg1), Arg2);
are_equal(Arg1, Arg2) when is_binary(Arg2) ->
    are_equal(Arg1, binary_to_list(Arg2));
are_equal(Arg1, Arg2) when is_integer(Arg1) ->
    are_equal(integer_to_list(Arg1), Arg2);
are_equal(Arg1, Arg2) when is_integer(Arg2) ->
    are_equal(Arg1, integer_to_list(Arg2));
are_equal(_Arg1, _Arg2) ->
    false.


%% @doc Return the name used in the context of a hostname
%% @spec name_for_host(atom(), atom()) -> atom().
name_for_host(Name, Host) ->
    z_convert:to_atom(z_convert:to_list(Name) ++ [$$, z_convert:to_list(Host)]).
