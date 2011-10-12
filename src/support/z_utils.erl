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
	are_equal/2,
	assert/2,
    encode_value/2,
    decode_value/2,
    encode_value_expire/3,
    decode_value_expire/2,
	checksum/2,
	checksum_assert/3,
	coalesce/1,
	combine/2,
	combine_defined/2,
	decode/2,
	depickle/2,
	encode/2,
	f/1,
	f/2,
	get_seconds/0,
	group_by/3,
	group_proplists/2,
	hex_decode/1,
	hex_encode/1,
	index_proplist/2,
	nested_proplist/1,
	nested_proplist/2,
	get_nth/2,
	set_nth/3,
	is_empty/1,
	is_process_alive/1,
	is_true/1,
	js_escape/1,
	js_array/1,
	js_object/1,
	js_object/2,
	lib_dir/0,
	lib_dir/1,
	list_dir_recursive/1,
	name_for_host/2,
	only_digits/1,
	only_letters/1,
	is_iolist/1,
	is_proplist/1,
	os_escape/1,
	os_filename/1,
	pickle/2,
	prefix/2,
	prop_delete/2,
	prop_replace/3,
	randomize/1,
	randomize/2,
	replace1/3,
	split/2,
	split_in/2,
	url_path_encode/1,
	url_encode/1,
	url_decode/1,
	vsplit_in/2,
    now/0,
    now_msec/0,
    tempfile/0,
    temppath/0,
    url_reserved_char/1,
    url_unreserved_char/1,
    url_valid_char/1,
    flush_message/1,
    ensure_existing_module/1,
	generate_username/2
]).

%%% FORMAT %%%

f(S) -> f(S, []).
f(S, Args) -> lists:flatten(io_lib:format(S, Args)).


%% @doc Return an abspath to a directory relative to the application root.
%% This is used to prevent that we have to name the root dir "zotonic".
lib_dir() ->
	{ok, Path} = zotonic_app:get_path(),
	Path.
lib_dir(Dir) ->
	{ok, Path} = zotonic_app:get_path(),
	filename:join([Path, z_convert:to_list(Dir)]).


%% @doc Return the current tick count
now() ->
    {M,S,_M} = erlang:now(),
    M*1000000 + S.

now_msec() ->
    {M,S,Micro} = erlang:now(),
    M*1000000000 + S*1000 + Micro div 1000.

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


%% Encode value securely, for use in cookies.

%% 50 usec on core2duo 2GHz
encode_value(Value, Context) ->
    Salt = z_ids:id(),
	Secret = z_ids:sign_key(Context),
    base64:encode(
        term_to_binary({Value, Salt, crypto:sha_mac(Secret, term_to_binary([Value, Salt]))})
    ).

%% 23 usec on core2duo 2GHz
decode_value(Data, Context) ->
    Secret = z_ids:sign_key(Context),
    {Value, Salt, Sign} = binary_to_term(base64:decode(Data)),
    Sign = crypto:sha_mac(Secret, term_to_binary([Value, Salt])),
    Value.

encode_value_expire(Value, Date, Context) ->
    encode_value({Value, Date}, Context).

decode_value_expire(Data, Context) ->
    {Value, Expire} = decode_value(Data, Context),
    case Expire >= calendar:local_time() of
        false -> {error, expired};
        true -> {ok, Value}
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

url_encode(S) -> 
    %% @todo possible speedups for binaries
    mochiweb_util:quote_plus(S).

% hexdigit is from Mochiweb.

-define(PERCENT, 37).  % $\%

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

%%% URL PATH ENCODE %%%

%% url spec for path part
url_path_encode(L) when is_list(L) ->
    url_path_encode(L, []);
url_path_encode(L) ->
    url_path_encode(z_convert:to_list(L)).

url_path_encode([], Acc) ->
    lists:reverse(Acc);
url_path_encode([$/|R], Acc) ->
    url_path_encode(R, [$/|Acc]);
url_path_encode([C|R], Acc) when (C==$: orelse C==$@ orelse C==$& orelse C==$= orelse C==$+ orelse C==$$ orelse C==$ orelse C==$;) ->
    url_path_encode(R, [C|Acc]);
url_path_encode([C|R], Acc)->
    case url_unreserved_char(C) of
        true ->
            url_path_encode(R, [C|Acc]);
        false ->
            <<Hi:4, Lo:4>> = <<C>>,
            url_path_encode(R, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc])
    end.


%% @spec os_filename(String) -> String
%% @doc Simple escape function for filenames as commandline arguments.
%% foo/"bar.jpg -> "foo/\"bar.jpg"; on windows "foo\\\"bar.jpg" (both including quotes!)
os_filename(A) when is_binary(A) ->
    os_filename(binary_to_list(A));
os_filename(A) when is_list(A) ->
    os_filename(lists:flatten(A), []).

os_filename([], Acc) ->
    filename:nativename([$"] ++ lists:reverse(Acc) ++ [$"]);
os_filename([$\\|Rest], Acc) ->
    os_filename_bs(Rest, Acc);
os_filename([$"|Rest], Acc) ->
    os_filename(Rest, [$", $\\ | Acc]);
os_filename([C|Rest], Acc) ->
    os_filename(Rest, [C|Acc]).

os_filename_bs([$\\|Rest], Acc) ->
    os_filename(Rest, [$\\,$\\|Acc]);
os_filename_bs([$"|Rest], Acc) ->
    os_filename(Rest, [$",$\\,$\\,$\\|Acc]);
os_filename_bs([C|Rest], Acc) ->
    os_filename(Rest, [C,$\\|Acc]).


%% @spec os_escape(String) -> String
%% @doc Simple escape function for command line arguments
os_escape(undefined) ->
	[];
os_escape(A) when is_binary(A) ->
    os_escape(binary_to_list(A));
os_escape(A) when is_list(A) ->
    {Family, _} = os:type(),
    os_escape(Family, lists:flatten(A), []).

os_escape(_, [], Acc) ->
    lists:reverse(Acc);
os_escape(unix, [C|Rest], Acc) when
                (C >= $A andalso C =< $Z)
         orelse (C >= $a andalso C =< $z)
         orelse (C >= $0 andalso C =< $9)
         orelse C == $_
         orelse C == $.
         orelse C == $-
         orelse C == $+
         orelse C == $/
    ->
    os_escape(unix, Rest, [C|Acc]);
os_escape(unix, [C|Rest], Acc) when
                C >= 32
        orelse  C == $\r
        orelse  C == $\n
        orelse  C == $\t
    ->
    os_escape(unix, Rest, [C,$\\|Acc]);

%% Win32 escaping, see: http://www.microsoft.com/resources/documentation/windows/xp/all/proddocs/en-us/ntcmds_shelloverview.mspx
os_escape(win32, [C|Rest], Acc) when C == $&
         orelse C == $|
         orelse C == $;
         orelse C == $,
         orelse C == $%
         orelse C == $(
         orelse C == $)
         orelse C == $"
         orelse C == $'
         orelse C == $=
         orelse C == $^
         orelse C == 32
    ->
    os_escape(win32, Rest, [C,$^|Acc]);
os_escape(win32, [C|Rest], Acc) ->
    os_escape(win32, Rest, [C|Acc]).


url_decode(S) ->
    lists:reverse(url_decode(S, [])).

url_decode([], Acc) -> 
    Acc;
url_decode([$%, A, B|Rest], Acc) ->
    Ch = erlang:list_to_integer([A, B], 16),
    url_decode(Rest, [Ch|Acc]);
url_decode([$+|Rest], Acc) ->
    url_decode(Rest, [32|Acc]);
url_decode([Ch|Rest], Acc) ->
    url_decode(Rest, [Ch|Acc]).

%% VALID URL CHARACTERS
%% RFC 3986
url_valid_char(Char) ->
    url_reserved_char(Char) orelse url_unreserved_char(Char).

url_reserved_char($!) -> true;
url_reserved_char($*) -> true;
url_reserved_char($") -> true;
url_reserved_char($') -> true;
url_reserved_char($() -> true;
url_reserved_char($)) -> true;
url_reserved_char($;) -> true;
url_reserved_char($:) -> true;
url_reserved_char($@) -> true;
url_reserved_char($&) -> true;
url_reserved_char($=) -> true;
url_reserved_char($+) -> true;
url_reserved_char($$) -> true;
url_reserved_char($,) -> true;
url_reserved_char($/) -> true;
url_reserved_char($?) -> true;
url_reserved_char($%) -> true;
url_reserved_char($#) -> true;
url_reserved_char($[) -> true;
url_reserved_char($]) -> true;
url_reserved_char(_) -> false.

url_unreserved_char(Ch) when Ch >= $A andalso Ch < $Z + 1 -> true;
url_unreserved_char(Ch) when Ch >= $a andalso Ch < $z + 1 -> true;
url_unreserved_char(Ch) when Ch >= $0 andalso Ch < $9 + 1 -> true;
url_unreserved_char($-) -> true;
url_unreserved_char($_) -> true;
url_unreserved_char($.) -> true;
url_unreserved_char($~) -> true;
url_unreserved_char(_)  -> false.



%%% ESCAPE JAVASCRIPT %%%

%% @doc Javascript escape, see also: http://code.google.com/p/doctype/wiki/ArticleXSSInJavaScript

js_escape(undefined) -> [];
js_escape([]) -> [];
js_escape(<<>>) -> [];
js_escape(Value) when is_integer(Value) -> integer_to_list(Value);
js_escape(Value) when is_atom(Value) ->  js_escape(atom_to_list(Value), []);
js_escape(Value) when is_binary(Value) -> js_escape(binary_to_list(Value), []);
js_escape(Value) -> js_escape(Value, []).

js_escape([], Acc) -> lists:reverse(Acc);
js_escape([$\\|T], Acc) -> js_escape(T, [$\\,$\\|Acc]);
js_escape([$\n|T], Acc) -> js_escape(T, [$n,$\\|Acc]);
js_escape([$\r|T], Acc) -> js_escape(T, [$r,$\\|Acc]);
js_escape([$\t|T], Acc) -> js_escape(T, [$t,$\\|Acc]);
js_escape([$'|T], Acc) -> js_escape(T, [$7,$2,$x,$\\|Acc]);
js_escape([$"|T], Acc) -> js_escape(T, [$2,$2,$x,$\\|Acc]);
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

js_array(L) ->
    [ $[, combine($,,[ js_prop_value(undefined, V) || V <- L ]), $] ].

%% @doc Create a javascript object from a proplist
js_object([]) -> <<"{}">>;
js_object(L) -> js_object(L,[]).

js_object(L, []) -> js_object1(L, []);
js_object(L, [Key|T]) -> js_object(proplists:delete(Key,L), T).

%% recursively add all properties as object properties
js_object1([], Acc) ->
    [${, combine($,,lists:reverse(Acc)), $}];
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
js_prop_value(_, Int) when is_integer(Int) -> integer_to_list(Int);
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

is_iolist(C) when is_integer(C) andalso C >= 0 andalso C =< 255 -> true;
is_iolist(B) when is_binary(B) -> true;
is_iolist([H|L]) -> is_iolist(H) andalso is_iolist(L);
is_iolist(_) -> false.

is_proplist([]) -> true;
is_proplist([{K,_}|R]) when is_atom(K) -> is_proplist(R);
is_proplist(_) -> false.


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


%% @doc Check if a value is 'empty'
is_empty(undefined) -> true;
is_empty([]) -> true;
is_empty(<<>>) -> true;
is_empty(_) -> false.

%% @doc Check if the parameter could represent the logical value of "true"
is_true([$t|_T]) -> true;
is_true([$y|_T]) -> true;
is_true([$T|_T]) -> true;
is_true([$Y|_T]) -> true;
is_true("on") -> true;
is_true("ON") -> true;
is_true("1") -> true;

is_true(<<"true">>) -> true;
is_true(<<"yes">>) -> true;
is_true(<<"on">>) -> true;
is_true(<<"TRUE">>) -> true;
is_true(<<"YES">>) -> true;
is_true(<<"ON">>) -> true;
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


%% @doc Scan the props of a proplist, when the prop is a list with a $. characters in it then split the prop.
nested_proplist(Props) ->
    nested_proplist(Props, []).

nested_proplist([], Acc) ->
    lists:reverse(Acc);
nested_proplist([{K,V}|T], Acc) when is_list(K) ->
    case string:tokens(K, ".") of
        [K0] -> nested_proplist(T, [{K0,V}|Acc]);
        List -> nested_proplist(T, nested_props_assign(List, V, Acc))
    end;
nested_proplist([H|T], Acc) ->
    nested_proplist(T, [H|Acc]).


    nested_props_assign([K], V, Acc) ->
        case only_digits(K) of
            true ->  set_nth(list_to_integer(K), V, Acc);
            false -> prop_replace(z_convert:to_atom(K), V, Acc)
        end;
    nested_props_assign([H|T], V, Acc) ->
        case only_digits(H) of
            true -> 
                Index = list_to_integer(H),
                NewV = case get_nth(Index, Acc) of
                           L when is_list(L) -> nested_props_assign(T, V, L);
                           _ -> nested_props_assign(T, V, [])
                       end,
                set_nth(Index, NewV, Acc);
            false ->
                K = z_convert:to_atom(H),
                NewV = case proplists:get_value(K, Acc) of
                    L when is_list(L) -> nested_props_assign(T, V, L);
                    _ -> nested_props_assign(T, V, [])
                end,
                prop_replace(K, NewV, Acc)
        end.


get_nth(N, L) when N >= 1 ->
    try lists:nth(N, L) catch _:_ -> undefined end.

set_nth(N, V, L) when N >= 1 ->
    try 
        case lists:split(N-1, L) of
            {Pre, []} -> Pre ++ [V];
            {Pre, [_|T]} -> Pre ++ [V|T]
        end
    catch _:_ ->
        set_nth(N, V, L ++ [undefined])
    end.



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
	vsplit_in(N, [], RunLength, Acc) ->
		vsplit_in(N-1, [], RunLength, [[]|Acc]);
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
%% @spec name_for_host(atom(), atom()) -> atom()
name_for_host(Name, Host) ->
    z_convert:to_atom(z_convert:to_list(Name) ++ [$$, z_convert:to_list(Host)]).


%% @doc Ensure that the given string matches an existing module. Used to prevent
%%      a denial of service attack where we exhaust the atom space.
ensure_existing_module(ModuleName) when is_list(ModuleName) ->
    case catch list_to_existing_atom(ModuleName) of
        {'EXIT', {badarg, _Traceback}} ->
            case code:where_is_file(ensure_valid_modulename(ModuleName) ++ ".beam") of
                non_existing -> {error, not_found};
                Absname ->
                    {module, Module} = code:load_abs(filename:rootname(Absname)),
                    {ok, Module}
            end;
        M ->
            ensure_existing_module(M)
     end;
ensure_existing_module(ModuleName) when is_atom(ModuleName) ->
    case module_loaded(ModuleName) of
        true -> 
            {ok, ModuleName};
        false ->
            {module, Module} = code:ensure_loaded(ModuleName),
            {ok, Module}
    end;
ensure_existing_module(ModuleName) when is_binary(ModuleName) ->
    ensure_existing_module(binary_to_list(ModuleName)).

    % Crash on a modulename that is not valid.
    ensure_valid_modulename(Name) ->
        lists:filter(fun ensure_valid_modulechar/1, Name).
            
        ensure_valid_modulechar(C) when C >= $0, C =< $9 -> true;
        ensure_valid_modulechar(C) when C >= $a, C =< $z -> true;
        ensure_valid_modulechar(C) when C >= $A, C =< $Z -> true;
        ensure_valid_modulechar(C) when C == $_ -> true.


%% @doc return a unique temporary filename.
%% @spec tempfile() -> string()
tempfile() ->
    {A,B,C}=erlang:now(),
    filename:join(temppath(), lists:flatten(io_lib:format("ztmp-~s-~p.~p.~p",[node(),A,B,C]))).


%% @doc Returns the path where to store temporary files.
%%@spec temppath() -> string()
temppath() ->
    lists:foldl(fun(false, Fallback) -> Fallback;
                   (Good, _) -> Good end,
                "/tmp",
                [os:getenv("TMP"), os:getenv("TEMP")]).


%% @doc Flush all incoming messages, used when receiving timer ticks to prevent multiple ticks.
flush_message(Msg) ->
    receive
        Msg -> flush_message(Msg)
    after 0 ->
        ok
    end.


%% @doc Generate a unique user name from a proplist.
generate_username(Props, Context) ->
    case proplists:get_value(title, Props) of
        [] ->
            First = proplists:get_value(name_first, Props),
            Last = proplists:get_value(name_surname, Props),
            generate_username1(z_string:nospaces(z_string:to_lower(First) ++ "." ++ z_string:to_lower(Last)), Context);
        Title ->
            generate_username1(z_string:nospaces(z_string:to_lower(Title)), Context)
    end.

generate_username1(Name, Context) ->
    case m_identity:lookup_by_username(Name, Context) of
        undefined -> Name;
        _ -> generate_username2(Name, Context)
    end.

generate_username2(Name, Context) ->
    N = integer_to_list(z_ids:number() rem 1000),
    case m_identity:lookup_by_username(Name++N, Context) of
        undefined -> Name;
        _ -> generate_username2(Name, Context)
    end.

