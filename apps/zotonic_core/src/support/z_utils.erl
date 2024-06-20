%% @author Marc Worrell
%% @copyright 2009-2023 Marc Worrell
%% @doc Misc utility functions for zotonic
%% Parts are from wf_utils.erl which is Copyright (c) 2008-2009 Rusty Klophaus
%% @end

%% Copyright 2009-2023 Marc Worrell
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

-export([
    pipeline/2,
    write_terms/2,
    get_value/2,
    get_value/3,
    are_equal/2,
    assert/2,
    encode_value/2,
    decode_value/2,
    encode_value_expire/3,
    decode_value_expire/2,
    checksum/2,
    checksum_assert/3,
    coalesce/1,
    join_defined/2,
    depickle/2,
    f/1,
    f/2,
    get_seconds/0,
    ranges/1,
    group_by/3,
    group_proplists/2,
    hex_decode/1,
    hex_encode/1,
    hex_sha/1,
    hex_sha2/1,
    index_proplist/2,
    nested_proplist/1,
    nested_proplist/2,
    get_nth/2,
    set_nth/3,
    is_empty/1,
    is_process_alive/1,
    erase_process_dict/0,
    is_true/1,
    js_escape/2,
    js_escape/1,
    js_array/1,
    js_object/1,
    js_object/2,
    js_object/3,
    lib_dir/0,
    lib_dir/1,
    wildcard/1,
    wildcard/2,
    wildcard_recursive/2,
    filter_dot_files/1,
    list_dir_recursive/1,
    name_for_site/2,
    only_digits/1,
    only_letters/1,
    is_iolist/1,
    is_proplist/1,
    os_escape/1,
    os_filename/1,
    pickle/2,
    prefix/2,
    hmac/3,
    prop_delete/2,
    prop_replace/3,
    props_merge/2,
    randomize/1,
    replace1/3,
    split/2,
    split_in/2,
    vsplit_in/2,
    now/0,
    now_msec/0,
    flush_message/1,
    ensure_existing_module/1
]).

%% @doc Apply a list of functions to a startlist of arguments.
%% All functions must return: ok | {ok, term()} | {error, term()}.
%% Execution stops if a function returns an error tuple.
%% The return value of the last executed function is returned.
-spec pipeline( list( PipelineFun ), list() ) -> ok | {ok, term()} | {error, term()}
    when PipelineFun :: function()
                      | mfa().
pipeline(Fs, As) ->
    pipeline_1(Fs, ok, As, length(As)).

pipeline_1(_Fs, {error, _} = Error, _As, _AsLen) ->
    Error;
pipeline_1([], Result, _As, _AsLen) ->
    Result;
pipeline_1([ F | Fs ], ok, As, AsLen) ->
    case pipeline_apply(F, As, AsLen) of
        nofun ->
            {error, F};
        R ->
            pipeline_1(Fs, R, As, AsLen)
    end;
pipeline_1([ F | Fs ], {ok, Result}, As, AsLen) ->
    FRes = case pipeline_apply(F, [ Result | As ], AsLen + 1) of
        nofun ->
            case pipeline_apply(F, As, AsLen) of
                nofun ->
                    {error, F};
                R ->
                    R
            end;
        R ->
            R
    end,
    pipeline_1(Fs, FRes, As, AsLen).

pipeline_apply(F, _As, _AsLen) when is_function(F, 0) ->
    F();
pipeline_apply(F, As, AsLen) when is_function(F, AsLen) ->
    erlang:apply(F, As);
pipeline_apply({M, F, A}, As, AsLen) when is_atom(M), is_atom(F), is_list(A) ->
    code:ensure_loaded(M),
    case erlang:function_exported(M, F, AsLen + length(A)) of
        false ->
            case erlang:function_exported(M, F, length(A)) of
                true ->
                    erlang:apply(M, F, A);
                false ->
                    nofun
            end;
        true ->
            erlang:apply(M, F, A ++ As)
    end;
pipeline_apply(_F, _As, _AsLen) ->
    nofun.


%% @doc Write a file that is readable by file:consult/1
-spec write_terms( file:filename_all(), list(term()) ) -> ok | {error, term()}.
write_terms(Filename, List) when is_list(List) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = unicode:characters_to_binary(lists:map(Format, List)),
    file:write_file(Filename, Text).


%% @doc Get a value from a map or a proplist. Return 'undefined' if
%% The value was not present.
-spec get_value( term(), map() | list() ) -> term().
get_value(Key, Map) when is_map(Map) ->
    maps:get(Key, Map, undefined);
get_value(Key, Map) when is_list(Map) ->
    proplists:get_value(Key, Map, undefined).

%% @doc Get a value from a map or a proplist. Return the default value if
%% The value was not present.
-spec get_value( term(), map() | list(), term() ) -> term().
get_value(Key, Map, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
get_value(Key, Map, Default) when is_list(Map) ->
    proplists:get_value(Key, Map, Default).


%%% FORMAT %%%
f(S) -> f(S, []).
f(S, Args) -> iolist_to_binary( io_lib:format(S, Args) ).


%% @doc Return an abspath to a directory relative to the application root.
%% @todo OTP check the "apps", that is wrong in the context of non zotonic-git projects
lib_dir() ->
    z_path:get_path().

lib_dir(modules) ->
    filename:join([lib_dir(), "apps"]);
lib_dir(Dir) ->
    filename:join([lib_dir(), Dir]).

%% @doc filename:wildcard version which filters dotfiles like unix does
-spec wildcard( DirName::file:filename_all() ) -> list( file:filename() ).
wildcard(DirName) when is_binary(DirName) ->
    wildcard(unicode:characters_to_list(DirName));
wildcard(Wildcard) ->
    filter_dot_files(filelib:wildcard(Wildcard)).

-spec wildcard( WildCard::string(), DirName::file:filename_all() ) -> list( file:filename() ).
wildcard(WildCard, DirName) when is_binary(DirName) ->
    wildcard(WildCard, unicode:characters_to_list(DirName));
wildcard(Wildcard, DirName) ->
    Fs = filter_dot_files(filelib:wildcard(Wildcard, DirName)),
    [ filename:join(DirName, F) || F <- Fs ].

-spec wildcard_recursive( WildCard::string(), DirName::file:filename_all() ) -> list( file:filename() ).
wildcard_recursive(WildCard, DirName) when is_binary(DirName) ->
    wildcard_recursive(WildCard, unicode:characters_to_list(DirName));
wildcard_recursive(WildCard, DirName) ->
    wildcard_recursive_1(WildCard, DirName, []).

wildcard_recursive_1(WildCard, Dirname, Found) ->
    Files = wildcard(WildCard, Dirname),
    All = wildcard("*", Dirname),
    Found1 = lists:foldl(
        fun (File, Acc) ->
            Path = filename:join(Dirname, File),
            case filelib:is_regular(Path) of
                true -> [ Path | Acc ];
                false -> Acc
            end
        end,
        Found,
        Files),
    lists:foldl(
        fun(MaybeDir, Acc) ->
            MaybeDirPath = filename:join(Dirname, MaybeDir),
            case filelib:is_dir(MaybeDirPath) of
                true ->
                    wildcard_recursive_1(WildCard, MaybeDirPath, Acc);
                false ->
                    Acc
            end
        end,
        Found1,
        All).

%% @doc Filter all filenames which start with a dot.
filter_dot_files(Names) ->
    [NoDotName || NoDotName <- Names, no_dot_file(NoDotName)].

no_dot_file("." ++ _) ->
    false;
no_dot_file(Name) ->
    no_dot_file1(filename:split(Name)).

no_dot_file1([]) -> true;
no_dot_file1(["." ++ _ | _]) -> false;
no_dot_file1([_|Rest]) -> no_dot_file1(Rest).


%% @doc Return a list of all files in a directory, recursive depth first search for files not starting with a '.'
list_dir_recursive(Dir) ->
    Sep = separator(element(1, os:type())),
    AbsDir = filename:absname(Dir),
    case file:list_dir(AbsDir) of
        {ok, Files} ->
            list_dir_recursive(Files, AbsDir, Sep, []);
        {error, _} ->
            []
    end.

list_dir_recursive([], _BaseDir, _Sep, Acc) ->
    Acc;
list_dir_recursive([[$.|_]|OtherFiles], BaseDir, Sep, Acc) ->
    list_dir_recursive(OtherFiles, BaseDir, Sep, Acc);
list_dir_recursive([File|OtherFiles], BaseDir, Sep, Acc) ->
    Path = [BaseDir, Sep, File],
    case filelib:is_regular(Path) of
        true ->
            list_dir_recursive(OtherFiles, BaseDir, Sep, [File|Acc]);
        false ->
            case filelib:is_dir(Path) of
                true ->
                    Acc1 = case file:list_dir(Path) of
                               {ok, Files} ->
                                   RelFiles = [ [File, Sep, F] || [H|_]=F <- Files, H /= $.],
                                   list_dir_recursive(RelFiles, BaseDir, Sep, Acc);
                               {error, _} ->
                                   Acc
                           end,
                    list_dir_recursive(OtherFiles, BaseDir, Sep, Acc1);
                false ->
                    list_dir_recursive(OtherFiles, BaseDir, Sep, Acc)
            end
    end.

separator(win32) -> $\\;
separator(_) -> $/.


%% @doc Return the current tick count
now() ->
    {M,S,_M} = os:timestamp(),
    M*1000000 + S.

now_msec() ->
    {M,S,Micro} = os:timestamp(),
    M*1000000000 + S*1000 + Micro div 1000.


%% @doc Return the current universal time in seconds
get_seconds() -> calendar:datetime_to_gregorian_seconds(calendar:universal_time()).


%% @doc Multinode is_process_alive check
is_process_alive(Pid) ->
    case is_pid(Pid) of
        true ->
            %% If node(Pid) is down, rpc:call returns something other than true or false.
            case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
                true -> true;
                _ -> false
            end;
        _ -> false
    end.


%% @doc Safe erase of process dict, keeps some 'magical' proc_lib vars
erase_process_dict() ->
    Values = [ {K, erlang:get(K)} || K <- ['$initial_call', '$ancestors', '$erl_eval_max_line'] ],
    erlang:erase(),
    [ erlang:put(K,V) || {K,V} <- Values, V =/= undefined ],
    ok.


%% @doc Encode value to a binary with a checksum, for use in cookies.
%% @deprecated Use z_crypto:encode_value/2 instead.
encode_value(Value, ContextOrSecret) ->
    z_crypto:encode_value(Value, ContextOrSecret).

%% @doc Decode a value. Crash if the checksum is invalid.
%% @deprecated Use z_crypto:decode_value/2 instead.
decode_value(Data, ContextOrSecret) ->
    z_crypto:decode_value(Data, ContextOrSecret).

%% @doc Encode a value using a checksum, add a date to check for expiration.
%% @deprecated Use z_crypto:encode_value_expire/3 instead.
-spec encode_value_expire(Value, Date, Context) -> Encoded when
    Value :: term(),
    Date :: calendar:datetime(),
    Context :: z:context(),
    Encoded :: binary().
encode_value_expire(Value, Date, Context) ->
    z_crypto:encode_value_expire(Value, Date, Context).

%% @doc Decode a value using a checksum, check date to check for expiration. Crashes
%% if the checksum is invalid.
%% @deprecated Use z_crypto:decode_value_expire/3 instead.
-spec decode_value_expire(Encoded, Context) -> {ok, Value} | {error, expired} when
    Encoded :: binary(),
    Value :: term(),
    Context :: z:context().
decode_value_expire(Data, Context) ->
    z_crypto:decode_value_expire(Data, Context).

%% @deprecated Use crypto:mac/4 instead.
hmac(Type, Key, Data) ->
    crypto:mac(hmac, Type, Key, Data).

%%% CHECKSUM %%%

%% @doc Calculate a checksum for the given data using the sign_key_simple of the site.
%% @deprecated Use z_crypto:checksum/2 instead.
-spec checksum(Data, Context) -> Checksum when
    Data :: iodata(),
    Context :: z:context(),
    Checksum :: binary().
checksum(Data, Context) ->
    z_crypto:checksum(Data, Context).

%% @doc Assert that the checksum is correct. Throws an exception of class error with
%% reason checksum_invalid if the checksum is not valid. The sign_key_simple if used
%% for the checksum calculation.
%% @deprecated Use z_crypto:checksum_assert/3 instead.
-spec checksum_assert(Data, Checksum, Context) -> ok | no_return() when
    Data :: iodata(),
    Checksum :: binary() | string(),
    Context :: z:context().
checksum_assert(Data, Checksum, Context) when is_binary(Checksum )->
    z_crypto:checksum_assert(Data, Checksum, Context).

%%% PICKLE / UNPICKLE %%%

%% @doc Encode an arbitrary to a binary. A checksum is added to prevent
%% decoding erlang terms not originating from this server. An Nonce is
%% added so that identical terms vary in their checksum. The encoded value
%% is safe to use in URLs (base64url). The site's sign_key is used as the
%% secret.
%% @deprecated Use z_crypto:pickle/2 instead.
-spec pickle(Term, Context) -> Data when
    Term :: term(),
    Context :: z:context(),
    Data :: binary().
pickle(Data, Context) ->
    z_crypto:pickle(Data, Context).

%% @doc Decode pickled base64url data. If the data checksum is invalid then an exception
%% of class error with reason {checksum_invalid, Data} is thrown. The site's sign_key is
%% used as the secret.
%% @deprecated Use z_crypto:depickle/2 instead.
-spec depickle(Data, Context) -> Term | no_return() when
    Data :: binary(),
    Context :: z:context(),
    Term :: term().
depickle(Data, Context) ->
    z_crypto:depickle(Data, Context).


%%% HEX ENCODE and HEX DECODE

-spec hex_encode(iodata()) -> binary().
hex_encode(Value) -> z_url:hex_encode(Value).

-spec hex_decode(iodata()) -> binary().
hex_decode(Value) -> z_url:hex_decode(Value).

%% @doc Hash data and encode into a hex string safe for filenames and texts.
-spec hex_sha(Value) -> Hash when
    Value :: iodata(),
    Hash :: binary().
hex_sha(Value) ->
    z_url:hex_encode_lc(crypto:hash(sha, Value)).

%% @doc Hash256 data and encode into a hex string safe for filenames and texts.
-spec hex_sha2(Value) -> Hash when
    Value :: iodata(),
    Hash :: binary().
hex_sha2(Value) ->
    z_url:hex_encode_lc(crypto:hash(sha256, Value)).

%% @doc Simple escape function for filenames as commandline arguments.
%% foo/"bar.jpg -> "foo/\"bar.jpg"; on windows "foo\\\"bar.jpg" (both including quotes!)
-spec os_filename( string()|binary() ) -> string().
os_filename(F) ->
    z_filelib:os_filename(F).

%% @doc Simple escape function for command line arguments
-spec os_escape(string()|binary()|undefined) -> string().
os_escape(S) ->
    z_filelib:os_escape(S).

%%% ESCAPE JAVASCRIPT %%%

%% @doc Javascript escape, see also: http://code.google.com/p/doctype/wiki/ArticleXSSInJavaScript
js_escape(#trans{ tr = [] }, _OptContext) -> [];
js_escape(#trans{} = Tr, OptContext) -> js_escape(z_trans:lookup_fallback(Tr, OptContext), OptContext);
js_escape({trust, Value}, _Context) -> Value;
js_escape(undefined, _OptContext) -> [];
js_escape([], _OptContext) -> [];
js_escape(<<>>, _OptContext) -> [];
js_escape(Value, _OptContext) when is_integer(Value) -> integer_to_list(Value);
js_escape(Value, OptContext) when is_atom(Value) ->  js_escape1(atom_to_list(Value), [], OptContext);
js_escape(Value, OptContext) when is_binary(Value) -> js_escape1(binary_to_list(Value), [], OptContext);
js_escape(Value, OptContext) -> js_escape1(Value, [], OptContext).

js_escape(V) ->
    js_escape(V, undefined).

js_escape1([], Acc, _OptContext) -> lists:reverse(Acc);
js_escape1([$\\|T], Acc, OptContext) -> js_escape1(T, [$\\,$\\|Acc], OptContext);
js_escape1([$\n|T], Acc, OptContext) -> js_escape1(T, [$n,$\\|Acc], OptContext);
js_escape1([$\r|T], Acc, OptContext) -> js_escape1(T, [$r,$\\|Acc], OptContext);
js_escape1([$\t|T], Acc, OptContext) -> js_escape1(T, [$t,$\\|Acc], OptContext);
js_escape1([$'|T], Acc, OptContext) -> js_escape1(T, [$7,$2,$x,$\\|Acc], OptContext);
js_escape1([$"|T], Acc, OptContext) -> js_escape1(T, [$2,$2,$x,$\\|Acc], OptContext);
js_escape1([$<|T], Acc, OptContext) -> js_escape1(T, [$c,$3,$x,$\\|Acc], OptContext);
js_escape1([$>|T], Acc, OptContext) -> js_escape1(T, [$e,$3,$x,$\\|Acc], OptContext);
js_escape1([$=|T], Acc, OptContext) -> js_escape1(T, [$d,$3,$x,$\\|Acc], OptContext);
js_escape1([$&|T], Acc, OptContext) -> js_escape1(T, [$6,$2,$x,$\\|Acc], OptContext);
%% js_escape1([16#85,C|T], Acc) when C >= 16#80 -> js_escape1(T, [C,16#85|Acc]);
%% js_escape1([16#85|T], Acc) -> js_escape1(T, [$5,$8,$0,$0,$u,$\\|Acc]);
js_escape1([16#2028|T],Acc, OptContext)-> js_escape1(T, [$8,$2,$0,$2,$u,$\\|Acc], OptContext);
js_escape1([16#2029|T],Acc, OptContext)-> js_escape1(T, [$9,$2,$0,$2,$u,$\\|Acc], OptContext);
js_escape1([16#e2,16#80,16#a8|T],Acc, OptContext)-> js_escape1(T, [$8,$2,$0,$2,$u,$\\|Acc], OptContext);
js_escape1([16#e2,16#80,16#a9|T],Acc, OptContext)-> js_escape1(T, [$9,$2,$0,$2,$u,$\\|Acc], OptContext);
js_escape1([H|T], Acc, OptContext) when is_integer(H) ->
    js_escape1(T, [H|Acc], OptContext);
js_escape1([H|T], Acc, OptContext) ->
    H1 = js_escape(H, OptContext),
    js_escape1(T, [H1|Acc], OptContext).

js_array(L) ->
    [ $[, lists:join($,,[ js_prop_value(undefined, V, undefined) || V <- L ]), $] ].


%% @doc Create a javascript object from a proplist
js_object(L) -> js_object(L, undefined).

js_object([], _OptContext) -> <<"{}">>;
js_object(L, OptContext) -> iolist_to_binary(js_object(L, [], OptContext)).

js_object(L, [], Context) -> js_object1(L, [], Context);
js_object(L, [Key|T], Context) -> js_object(proplists:delete(Key,L), T, Context).


%% recursively add all properties as object properties
js_object1([], Acc, _OptContext) ->
    [${, lists:join($,,lists:reverse(Acc)), $}];
js_object1([{Key,Value}|T], Acc, OptContext) ->
    Prop = [atom_to_list(Key), $:, js_prop_value(Key, Value, OptContext)],
    js_object1(T, [Prop|Acc], OptContext).


js_prop_value(_, undefined, _OptContext) -> <<"null">>;
js_prop_value(_, null, _OptContext) -> <<"null">>;
js_prop_value(_, true, _OptContext) -> <<"true">>;
js_prop_value(_, false, _OptContext) -> <<"false">>;
js_prop_value(_, Atom, _OptContext) when is_atom(Atom) -> [$",js_escape(erlang:atom_to_list(Atom)), $"];
js_prop_value(pattern, [$/|T]=List, OptContext) ->
    %% Check for regexp
    case length(T) of
        Len when Len =< 2 ->
            [$",js_escape(List, OptContext),$"];
        _Len ->
            case string:rchr(T, $/) of
                0 ->
                    [$",js_escape(List, OptContext),$"];
                N ->
                    {_Re, [$/|Options]} = lists:split(N-1,T),
                    case only_letters(Options) of
                        true -> List;
                        false -> [$",js_escape(List, OptContext),$"]
                    end
            end
    end;
js_prop_value(_, Int, _OptContext) when is_integer(Int) -> integer_to_list(Int);
js_prop_value(_, {trust, Value}, _OptContext) -> Value;
js_prop_value(_, Value, OptContext) -> [$",js_escape(Value, OptContext),$"].


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
    only_digits(binary_to_list(B));
only_digits(_) -> false.


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

join_defined(Sep, List) ->
    List2 = lists:filter(
        fun
            (undefined) -> false;
            (null) -> false;
            (_) -> true
        end,
        List),
    lists:join(Sep, List2).

prefix(Sep, List) -> prefix(Sep,List,[]).

prefix(_Sep, [], Acc) -> lists:reverse(Acc);
prefix(Sep, [H|T], Acc) -> prefix(Sep, T, [H,Sep|Acc]).


%% @doc COALESCE, select the first value non-null-ish value in a list.
%% Return 'undefined' if there are no non-null-ish values. A value is
%% is considered null-ish if it is undefined, null or the empty list.
-spec coalesce(List) -> Value when
    List :: list(),
    Value :: term().
coalesce([]) -> undefined;
coalesce([undefined|T]) -> coalesce(T);
coalesce([null|T]) -> coalesce(T);
coalesce([[]|T]) -> coalesce(T);
coalesce([H|_]) -> H.


%% @doc Check if a value is 'empty'. Special empty values are empty strings,
%% dates in the year 9999 and trans records with no or only empty values.
-spec is_empty(Value) -> boolean() when
    Value :: term().
is_empty(undefined) -> true;
is_empty(null) -> true;
is_empty([]) -> true;
is_empty(<<>>) -> true;
is_empty({{9999,_,_},{_,_,_}}) -> true;
is_empty(#trans{ tr = [] }) -> true;
is_empty(#trans{ tr = Tr }) ->
    lists:all(fun({_,Text}) -> Text =:= <<>> end, Tr);
is_empty(_) -> false.


%% @doc Check if the parameter could represent the logical value of "true"
-spec is_true(Value) -> boolean() when
    Value :: string() | binary() | boolean() | on | yes | number() | term().
is_true([$t|_T]) -> true;
is_true([$y|_T]) -> true;
is_true([$T|_T]) -> true;
is_true([$Y|_T]) -> true;
is_true("on") -> true;
is_true("ON") -> true;
is_true("1") -> true;

is_true(<<"t", _/binary>>) -> true;
is_true(<<"y", _/binary>>) -> true;
is_true(<<"T", _/binary>>) -> true;
is_true(<<"Y", _/binary>>) -> true;
is_true(<<"on">>) -> true;
is_true(<<"ON">>) -> true;
is_true(<<"1">>) -> true;

is_true(true) -> true;
is_true(yes) -> true;
is_true(on) -> true;

is_true(N) when is_integer(N) andalso N =/= 0 -> true;
is_true(0.0) -> false;
is_true(-0.0) -> false;
is_true(N) when is_float(N) -> true;

is_true(_) -> false.


%% @doc Check if an assertion is ok or failed, raise an erlang error
%% exception if the condition failed.
-spec assert(Condition, Error) -> ok when
    Condition :: boolean(),
    Error :: term().
assert(false, Error) -> erlang:error(Error);
assert(_, _) -> ok.


%% @doc Replace a property in a proplist with a new value.
-spec prop_replace(Prop, Value, List) -> List1 when
    Prop :: term(),
    Value :: term(),
    List :: proplists:proplist(),
    List1 :: proplists:proplist().
prop_replace(Prop, Value, List) ->
    [{Prop,Value} | lists:keydelete(Prop,1,List)].

prop_delete(Prop, List) ->
    lists:keydelete(Prop, 1, List).

%% @doc Overlay property list List1 over List2, keys in List1 overrule
%% keys in List2.
-spec props_merge(List1, List2) -> List3 when
    List1 :: proplists:proplist(),
    List2 :: proplists:proplist(),
    List3 :: proplists:property().
props_merge([], Ps) ->
    Ps;
props_merge(Ps, []) ->
    Ps;
props_merge(Ps, [{K,_}=P|Xs]) ->
    case proplists:is_defined(K, Ps) of
        true -> props_merge(Ps, Xs);
        false -> props_merge([P|Ps], Xs)
    end.


%% @doc Given a list of proplists, make it a nested list with respect to a property, combining elements
%% with the same property.  Assumes the list is sorted on the property you are splitting on
%% For example:  [[{a,b}{x}], [{a,b}{z}], [{a,c}{y}]] gives:
%%   [ {b, [[{a,b}{x}], [{a,b}{z}]]},  {c, [[{a,c}{y}]]} ]
-spec group_proplists(atom(), [{atom(), term()}]) -> list({term(), list()}).
group_proplists(_Prop, []) ->
    [];
group_proplists(Prop, [Item|Rest]) ->
    PropValue = proplists:get_value(Prop, Item),
    group_proplists(Prop, PropValue, Rest, [Item], []).

group_proplists(_Prop, _PropValue, [], [], Result) ->
    lists:reverse(Result);
group_proplists(Prop, PropValue, [], Acc, Result) ->
    Acc1 = lists:reverse(Acc),
    group_proplists(Prop, PropValue, [], [], [{PropValue,Acc1}|Result]);
group_proplists(Prop, PropValue, [C|Rest], Acc, Result) ->
    case proplists:get_value(Prop, C) of
        PropValue ->
            group_proplists(Prop, PropValue, Rest, [C|Acc], Result);
        Other ->
            Acc1 = lists:reverse(Acc),
            group_proplists(Prop, Other, Rest, [C], [{PropValue,Acc1}|Result])
    end.


%% @doc Make a property list based on the value of a property
%% For example:  [  [{a,b}], [{a,c}] ]  gives  [{a, [{a,b}]}, {c, [[{a,c}]]}]
-spec index_proplist(term(), list({term(), term()})) -> list({term(), term()}).
index_proplist(_Prop, []) -> [];
index_proplist(Prop, List) ->
    index_proplist(Prop, List, []).

index_proplist(_Prop, [], Acc) ->
    lists:reverse(Acc);
index_proplist(Prop, [L|Rest], Acc) ->
    index_proplist(Prop, Rest, [{proplists:get_value(Prop,L),L}|Acc]).


%% @doc Scan the props of a proplist, when the prop is a string with "." characters in it then split the prop.
nested_proplist(Props) ->
    nested_proplist(Props, []).

nested_proplist([], Acc) ->
    lists:reverse(Acc);
nested_proplist([{K,V}|T], Acc) when is_list(K) ->
    case string:tokens(K, ".") of
        [K0] -> nested_proplist(T, [{K0,V}|Acc]);
        List -> nested_proplist(T, nested_props_assign(List, V, Acc))
    end;
nested_proplist([{K,V}|T], Acc) when is_binary(K) ->
    case binary:split(K, <<".">>, [global]) of
        [K0] -> nested_proplist(T, [{K0,V}|Acc]);
        List -> nested_proplist(T, nested_props_assign(List, V, Acc))
    end;
nested_proplist([H|T], Acc) ->
    nested_proplist(T, [H|Acc]).

nested_props_assign([K], V, Acc) ->
    case only_digits(K) of
        true ->  set_nth(list_to_integer(K), V, Acc);
        false -> prop_replace(K, V, Acc)
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
            NewV = case proplists:get_value(H, Acc) of
                       L when is_list(L) -> nested_props_assign(T, V, L);
                       _ -> nested_props_assign(T, V, [])
                   end,
            prop_replace(H, NewV, Acc)
    end.


%% @doc Get the Nth value of a list, if the list is too short then return 'undefined'.
%% The first value is 1.
-spec get_nth(N, L) -> Value | undefined when
    N :: non_neg_integer(),
    L :: list(),
    Value :: term().
get_nth(N, L) when N >= 1 ->
    try lists:nth(N, L) catch _:_ -> undefined end;
get_nth(_N, _L) ->
    undefined.

%% @doc Update the nth value of a list. The first value is 1. If the list is too
%% short then it is appended with 'undefined' values till the correct length.
-spec set_nth(N, Value, List) -> List2 when
    N :: pos_integer(),
    Value :: term(),
    List :: list(),
    List2 :: list().
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
-spec randomize(list()) -> list().
randomize(List) ->
    D = lists:map(fun(A) ->
                     {rand:uniform(1000000), A}
                  end, List),
    {_, D1} = lists:unzip(lists:keysort(1, D)),
    D1.

%% @doc Take max N elements from a list.
-spec split(integer(), list()) -> {list(), list()}.
split(N, L) ->
    split(N, L, []).

split(_N, [], Acc) ->
    {lists:reverse(Acc), []};
split(N, Rest, Acc) when N =< 0 ->
    {lists:reverse(Acc), Rest};
split(N, [A | Rest], Acc) ->
    split(N - 1, Rest, [A | Acc]).


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


%% @doc Convert a sorted list of integers to a list of range pairs {From,To}
-spec ranges([integer()]) -> [ {integer(),integer()} ].
ranges([]) ->
    [];
ranges([N|Ns]) ->
    ranges(Ns, [{N,N}]).

ranges([],Acc) ->
    lists:reverse(Acc);
ranges([N|Ns], [{A,B}|Acc]) when B+1 =:= N ->
    ranges(Ns, [{A,N}|Acc]);
ranges([N|Ns], Acc) ->
    ranges(Ns, [{N,N}|Acc]).

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
-spec name_for_site(Name :: atom(), atom() | #context{}) -> atom().
name_for_site(Name, #context{} = Context) ->
    name_for_site(Name, z_context:site(Context));
name_for_site(Name, Site) when is_atom(Site) ->
    z_convert:to_atom(z_convert:to_list(Name) ++ [$$, z_convert:to_list(Site)]).


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
            case code:ensure_loaded(ModuleName) of
                {module, Module} -> {ok, Module};
                {error, E} -> {error, E}
            end
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


%% @doc Flush all incoming messages, used when receiving timer ticks to prevent multiple ticks.
flush_message(Msg) ->
    receive
        Msg -> flush_message(Msg)
    after 0 ->
            ok
    end.
