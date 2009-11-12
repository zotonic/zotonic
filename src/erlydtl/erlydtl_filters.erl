%%%-------------------------------------------------------------------
%%% File:      erlydtl_filters.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc 
%%% Template filters
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2007-11-11 by Roberto Saccon, Evan Miller
%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
%%% Adapted and expanded for Zotonic by Marc Worrell <marc@worrell.nl>
%%%-------------------------------------------------------------------

-module(erlydtl_filters).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').

-compile(export_all).

-include_lib("zotonic.hrl").

-define(NO_ENCODE(C), ((C >= $a andalso C =< $z) orelse
                                  (C >= $A andalso C =< $Z) orelse
                                  (C >= $0 andalso C =< $9) orelse
                                  (C =:= $. orelse C =:= $- 
                                  orelse C =:= $~ orelse C =:= $_))).

opttrans({trans, _}=Trans, LanguageOrContext) ->
	z_trans:trans(Trans, LanguageOrContext);
opttrans(V, _LanguageOrContext) ->
	V.

add(undefined, _Number, _Context) ->
    undefined;
add(Input, Number, Context) when is_binary(Input) ->
    list_to_binary(add(binary_to_list(Input), Number, Context));
add(Input, Number, Context) when is_list(Input) ->
    integer_to_list(add(list_to_integer(Input), Number, Context));
add(Input, Number, _Context) when is_integer(Input) ->
    Input + z_convert:to_integer(Number).


append(Input, undefined, _Context) ->
    Input;
append(undefined, Append, _Context) ->
    Append;
append(Input, Append, _Context) ->
    z_convert:to_list(Input) ++ z_convert:to_list(Append).


insert(Input, Insert, Context) ->
    append(Insert, Input, Context).


lt(undefined, _Number, _Context) ->
    undefined;
lt(Input, Number, _Context) ->
    try
        z_convert:to_integer(Input) < z_convert:to_integer(Number)
    catch
        _:_ -> undefined
    end.

le(undefined, _Number, _Context) ->
    undefined;
le(Input, Number, _Context) ->
    try
        z_convert:to_integer(Input) =< z_convert:to_integer(Number)
    catch
        _:_ -> undefined
    end.

eq(undefined, _Number, _Context) ->
    undefined;
eq(Input, Number, _Context) ->
    try
        z_convert:to_list(Input) == z_convert:to_list(Number)
    catch
        _:_ -> undefined
    end.


ne(undefined, _Number, _Context) ->
    undefined;
ne(Input, Number, _Context) ->
    try
        z_convert:to_list(Input) /= z_convert:to_list(Number)
    catch
        _:_ -> undefined
    end.


eq_day({Y,M,D}, {Y,M,D}, _Context) ->
    true;
eq_day({{Y,M,D}, _}, {{Y,M,D}, _}, _Context) ->
    true;
eq_day(_, _, _Context) ->
    false.

ne_day(A, B, Context) ->
    not eq_day(A,B, Context).


b_not(Input, _Context) ->
    not z_convert:to_bool(Input).


b_or(Input, Parm, _Context) ->
    z_convert:to_bool(Input) or z_convert:to_bool(Parm).


b_xor(Input, Parm, _Context) ->
    z_convert:to_bool(Input) xor z_convert:to_bool(Parm).


b_and(Input, Parm, _Context) ->
    z_convert:to_bool(Input) and z_convert:to_bool(Parm).


ornot(Input, Parm, _Context) ->
    z_convert:to_bool(Input) or not z_convert:to_bool(Parm).


andnot(Input, Parm, _Context) ->
    z_convert:to_bool(Input) and not z_convert:to_bool(Parm).


notor(Input, Parm, _Context) ->
    not z_convert:to_bool(Input) or z_convert:to_bool(Parm).


notand(Input, Parm, _Context) ->
    not z_convert:to_bool(Input) and z_convert:to_bool(Parm).


capfirst(undefined, _Context) ->
    undefined;
capfirst([H|T], _Context) when H >= $a andalso H =< $z ->
    [H + $A - $a | T];
capfirst(<<Byte:8/integer, Binary/binary>>, _Context) when Byte >= $a andalso Byte =< $z ->
    [<<(Byte + $A - $a)>>, Binary].

center(undefined, _Number, _Context) ->
    undefined;
center(Input, Number, Context) when is_binary(Input) ->
    list_to_binary(center(binary_to_list(Input), Number, Context));
center(Input, Number, _Context) when is_list(Input) ->
    string:centre(Input, z_convert:to_integer(Number)).


date_range([A, B], [WithDate, Sep, EqDate], Context) ->
    case eq_day(A, B, Context) of
        true -> [ date(A, WithDate, Context), Sep, date(B, EqDate, Context) ];
        false -> [ date(A, WithDate, Context), Sep, date(B, WithDate, Context) ]
    end.

date(undefined, _FormatStr, _Context) ->
    undefined;
date(Input, FormatStr, Context) when is_binary(FormatStr) ->
    date(Input, binary_to_list(FormatStr), Context);
date(Input, FormatStr, Context) when is_binary(Input) ->
    list_to_binary(date(binary_to_list(Input), FormatStr, Context));
date({{_,_,_} = Date,{_,_,_} = Time}, FormatStr, _Context) ->
     erlydtl_dateformat:format({Date, Time}, FormatStr);
date({_,_,_} = Date, FormatStr, _Context) ->
    erlydtl_dateformat:format(Date, FormatStr);
date(Input, _FormatStr, _Context) when is_list(Input) ->
    io:format("Unexpected date parameter : ~p~n", [Input]),
    "".

escapejs(undefined, _Context) ->
    <<>>;
escapejs(Input, _Context) when is_binary(Input) ->
    z_utils:js_escape(Input);
escapejs(Input, _Context) when is_list(Input) ->
    z_utils:js_escape(Input).

escapexml(B, _Context) ->
    z_xml:escape(B).

first(undefined, _Context) ->
    undefined;
first([First|_Rest], _Context) ->
    [First];
first(<<First, _/binary>>, _Context) ->
    <<First>>;
first(_Other, _Context) ->
    <<>>.

fix_ampersands(undefined, _Context) ->
    undefined;
fix_ampersands(Input, _Context) when is_binary(Input) ->
    fix_ampersands1(Input, 0);
fix_ampersands(Input, _Context) when is_list(Input) ->
    fix_ampersands1(Input, []).

force_escape(undefined, _Context) -> 
    <<>>;
force_escape(Input, _Context) when is_atom(Input) ->
    escape1(atom_to_list(Input), []);
force_escape(Input, _Context) when is_list(Input) ->
    escape1(Input, []);
force_escape(Input, _Context) when is_binary(Input) ->
    escape1(Input, 0);
force_escape(Input, _Context) when is_integer(Input) ->
    integer_to_list(Input);
force_escape({{Y,M,D}, {_H,_I,_S}} = Input, Context) when is_integer(Y) andalso is_integer(M) andalso is_integer(D) ->
    date(Input, "Y-m-d H:i:s", Context);
force_escape({Y,M,D} = Input, Context) when is_integer(Y) andalso is_integer(M) andalso is_integer(D) ->
    date(Input, "Y-m-d", Context);
force_escape(true, _Context) ->
    yesno(true, _Context);
force_escape(false, _Context) ->
    yesno(false, _Context).
    

format_integer(Input, _Context) when is_integer(Input) ->
    integer_to_list(Input);
format_integer(Input, _Context) ->
    Input.

format_number(Input, _Context) when is_integer(Input) ->
    integer_to_list(Input);
format_number(Input, _Context) when is_float(Input) ->
    io_lib:format("~p", [Input]);
format_number(Input, Context) when is_function(Input, 0) ->
    format_number(Input(), Context);
format_number(Input, Context) when is_function(Input, 1) ->
    format_number(Input(Context), Context);
format_number(Input, _Context) ->
    Input.

format_price(Input, _Context) when is_integer(Input) ->
    case Input rem 100 of
        0 -> 
            integer_to_list(Input div 100);
        Cents when Cents < 10 -> 
            [integer_to_list(Input div 100), $,, $0, Cents + $0 ];
        Cents -> 
            [integer_to_list(Input div 100), $,, integer_to_list(Cents) ]
    end;
format_price(Input, Context) when is_float(Input) ->
    format_price(round(Input * 100), Context);
format_price(Input, Context) when is_function(Input, 0) ->
    format_price(Input(), Context);
format_price(Input, Context) when is_function(Input, 1) ->
    format_price(Input(Context), Context);
format_price(Input, Context) when is_list(Input) ->
    case string:to_integer(Input) of
        {error, _} -> Input;
        {N, _Rest} -> format_price(N, Context)
    end;
format_price(undefined, _Context) ->
    "-".

% default, default_if_none and default_if_undefined are directly compiled inline.

is_defined(undefined, _Context) ->
    false;
is_defined(_V, _Context) ->
    true.

is_undefined(V, Context) ->
    not(is_defined(V, Context)).


split_in(undefined, _N, _Context) ->
    undefined;
split_in(In, N, Context) ->
    z_utils:split_in(erlydtl_runtime:to_list(In, Context), N).


vsplit_in(undefined, _N, _Context) ->
    undefined;
vsplit_in(In, N, Context) ->
    z_utils:vsplit_in(erlydtl_runtime:to_list(In, Context), N).


striptags(undefined, _Context) ->
    undefined;
striptags(In, _Context) when is_integer(In) ->
    In;
striptags(In, _Context) when is_float(In) ->
    In;
striptags(In, _Context) ->
    z_html:strip(In).
    

% Translate atoms and numbers to strings
% Leave tuples as tuples.
stringify(undefined, _Context) ->
    <<>>;
stringify(In, _Context) when is_atom(In) ->
    atom_to_list(In);
stringify(In, _Context) when is_integer(In) ->
    integer_to_list(In);
stringify(In, _Context) when is_float(In) ->
    mochinum:digits(In);
stringify(In, _Context) ->
    In.

slugify(undefined, _Context) ->
    undefined;
slugify(Input, _Context) ->
    z_string:to_slug(Input).


join(Input, Separator, Context) when is_binary(Input) ->
    join(binary_to_list(Input), Separator, Context);
join(Input, Separator, _Context) when is_list(Input) ->
    List1 = lists:map(fun(X) -> z_convert:to_list(X) end, Input),
    string:join(List1, z_convert:to_list(Separator));
join(Input, _, _Context) ->
    Input.

last(undefined, _Context) ->
    undefined;
last(Input, _Context) when is_binary(Input) ->
    case size(Input) of
        0 -> Input;
        N ->
            Offset = N - 1,
            <<_:Offset/binary, Byte/binary>> = Input,
            Byte
    end;
last(Input, _Context) when is_list(Input) ->
    [lists:last(Input)];
last(Input, _Context) ->
    Input.

length(undefined, _Context) ->
    undefined;
length([], _Context) -> 
    "0";
length(<<>>, _Context) -> 
    "0";
length(Input, _Context) when is_list(Input) ->
    integer_to_list(erlang:length(Input));
length(Input, _Context) when is_binary(Input) ->
    integer_to_list(size(Input));
length(_Input, _Context) ->
    "1".

length_is(undefined, _Number, _Context) ->
    undefined;
length_is(Input, Number, Context) when is_list(Input), is_integer(Number) ->
    length_is(Input, integer_to_list(Number), Context);
length_is(Input, Number, Context) when is_list(Input), is_list(Number) ->
    ?MODULE:length(Input, Context) =:= Number;
length_is(_Input, Number, _Context) ->
    1 =:= z_convert:to_integer(Number).


linebreaksbr(undefined, _Context) ->
    undefined;
linebreaksbr(Input, _Context) when is_binary(Input) ->
    linebreaksbr1(Input, 0);
linebreaksbr(Input, _Context) ->
    linebreaksbr1(Input, []).


group_by(In, undefined, _Context) -> 
    In;
group_by(undefined, _, _Context) -> 
    undefined;
group_by(In, Prop, Context) ->
    z_utils:group_by(erlydtl_runtime:to_list(In, Context), z_convert:to_atom(Prop), Context).


ljust(undefined, _Number, _Context) -> 
    undefined;
ljust(Input, Number, Context) when is_binary(Input) ->
    list_to_binary(ljust(binary_to_list(Input), Number, Context));
ljust(Input, Number, _Context) when is_list(Input) ->
    string:left(Input, z_convert:to_integer(Number));
ljust(Input, _Number, _Context) -> 
    Input.


lower(undefined, _Context) ->
    undefined;
lower(Input, _Context) when is_list(Input) or is_binary(Input) ->
    z_string:to_lower(Input);
lower(Input, _Context) ->
    Input.


make_list(In, Context) ->
    erlydtl_runtime:to_list(In, Context).


member(_S, undefined, _Context) ->
    false;
member(S, [H|_] = L, _Context) when is_list(S) andalso is_binary(H) ->
    lists:member(list_to_binary(S), L);
member(S, [H|_] = L, _Context) when is_list(S) andalso is_integer(H) ->
    try
        lists:member(list_to_integer(S), L)
    catch
        _:_ -> false
    end;
member(S, L, _Context) when is_list(L) ->
    lists:member(S, L);
member(_S, _L, _Context) ->
    undefined.


nthtail(In, N, Context) ->
    case erlydtl_runtime:to_list(In, Context) of
        L when is_list(L) ->
            try
                lists:nthtail(z_convert:to_integer(N), L)
            catch 
                _:_ -> []
            end;
        _ -> 
            []
    end.


rjust(undefined, _Number, _Context) -> 
    undefined;
rjust(Input, Number, Context) when is_binary(Input) ->
    list_to_binary(rjust(binary_to_list(Input), Number, Context));
rjust(Input, Number, _Context) when is_list(Input) ->
    string:right(Input, z_convert:to_integer(Number));
rjust(Input, _Number, _Context) -> 
    Input.


show_media(undefined, _Context) ->
    undefined;
show_media(Input, Context) when is_binary(Input) ->
    show_media1(Input, 0, Context);
show_media(Input, _Context) ->
    Input.


tail(In, Context) ->
    case erlydtl_runtime:to_list(In, Context) of
        [_|T] -> T;
        _ -> []
    end.

timesince(Date, Context) ->
	z_datetime:timesince(Date, Context).
timesince(Date, Base, Context) ->
	z_datetime:timesince(Date, Base, Context).
	
	
truncate(undefined, _N, _Context) ->
	undefined;
truncate(In, N, Context) ->
	z_string:truncate(erlydtl_runtime:to_list(In, Context), z_convert:to_integer(N)).


upper(undefined, _Context) ->
    undefined;
upper(Input, _Context) when is_list(Input) or is_binary(Input) ->
    z_string:to_upper(Input);
upper(Input, _Context) ->
    Input.

urlencode(undefined, _Context) ->
    undefined;
urlencode(Input, _Context) when is_binary(Input) ->
    urlencode1(Input, 0);
urlencode(Input, _Context) when is_list(Input) ->
    urlencode1(Input, []);
urlencode(_Input, _Context) ->
    <<>>.

yesno(B, _Context) ->
    case erlydtl_runtime:is_false(B) of
        true -> "no";
        false -> "yes"
    end.
yesno(undefined, Values, _Context) ->
    case string:tokens(z_convert:to_list(Values), ",") of
        [_Yes, _No, Maybe] -> Maybe;
        [_Yes, No] -> No
    end;
yesno(B, Values, _Context) ->
    case erlydtl_runtime:is_false(B) of
        true ->
            [_Yes,No|_Rest] = string:tokens(z_convert:to_list(Values), ","),
            No;
        false -> 
            [Yes|_Rest] = string:tokens(z_convert:to_list(Values), ","),
            Yes
    end.


% internal

escape1(Binary, Index) when is_binary(Binary) ->
    case Binary of
        <<Pre:Index/binary, $<, Post/binary>> ->
            process_binary_match(Pre, <<"&lt;">>, size(Post), escape1(Post, 0));
        <<Pre:Index/binary, $>, Post/binary>> ->
            process_binary_match(Pre, <<"&gt;">>, size(Post), escape1(Post, 0));
        <<Pre:Index/binary, $&, Post/binary>> ->
            process_binary_match(Pre, <<"&amp;">>, size(Post), escape1(Post, 0));
        <<Pre:Index/binary, 34, Post/binary>> ->
            process_binary_match(Pre, <<"&quot;">>, size(Post), escape1(Post, 0));
        <<Pre:Index/binary, 39, Post/binary>> ->
            process_binary_match(Pre, <<"&#039;">>, size(Post), escape1(Post, 0));
        <<_:Index/binary, _, _/binary>> ->
            escape1(Binary, Index + 1);
        Binary ->
            Binary
    end;
escape1([], Acc) ->
    lists:reverse(Acc);
escape1("<" ++ Rest, Acc) ->
    escape1(Rest, lists:reverse("&lt;", Acc));
escape1(">" ++ Rest, Acc) ->
    escape1(Rest, lists:reverse("&gt;", Acc));
escape1("&" ++ Rest, Acc) ->
    escape1(Rest, lists:reverse("&amp;", Acc));
escape1("\"" ++ Rest, Acc) ->
    escape1(Rest, lists:reverse("&quot;", Acc));
escape1("'" ++ Rest, Acc) ->
    escape1(Rest, lists:reverse("&#039;", Acc));
escape1([C | Rest], Acc) ->
    escape1(Rest, [C | Acc]).


fix_ampersands1(Input, Index) when is_binary(Input) ->
    case Input of
        <<Pre:Index/binary, $&, Post/binary>> ->
            process_binary_match(Pre, <<"&amp;">>, size(Post), Post);
        <<_:Index/binary, _/binary>> ->
            fix_ampersands1(Input, Index + 1);
        _ ->
            Input
    end;
fix_ampersands1([], Acc) ->
    lists:reverse(Acc);
fix_ampersands1("&" ++ Rest, Acc) ->
    fix_ampersands1(Rest, lists:reverse("&amp;", Acc));
fix_ampersands1([C | Rest], Acc) ->
    fix_ampersands1(Rest, [C | Acc]).

linebreaksbr1(Input, Index) when is_binary(Input) ->
    Break = <<"<br />">>,
    case Input of
        <<Pre:Index/binary, $\r, $\n, Post/binary>> ->
            process_binary_match(Pre, Break, size(Post), linebreaksbr1(Post, 0));
        <<Pre:Index/binary, $\n, Post/binary>> ->
            process_binary_match(Pre, Break, size(Post), linebreaksbr1(Post, 0));
        <<_:Index/binary, _/binary>> ->
            linebreaksbr1(Input, Index + 1);
        _ ->
            Input
    end;
linebreaksbr1([], Acc) ->
    lists:reverse(Acc);
linebreaksbr1("\r\n" ++ Rest, Acc) ->
    linebreaksbr1(Rest, lists:reverse("<br />", Acc));
linebreaksbr1("\n" ++ Rest, Acc) ->
    linebreaksbr1(Rest, lists:reverse("<br />", Acc));
linebreaksbr1([C | Rest], Acc) ->
    linebreaksbr1(Rest, [C | Acc]).



% scanning for media start marker
show_media1(Input, Index, Context) when is_binary(Input) ->
    case Input of
        <<Pre:Index/binary, "<!-- z-media ", Post/binary>> ->
            process_binary_match(Pre, <<>>, size(Post), show_media1_id(Post, 0, Context));
        <<_:Index/binary, _/binary>> ->
            show_media1(Input, Index + 1, Context);
        _ ->
            Input
    end.

% scanning for media id
show_media1_id(Input, Index, Context) ->
    case Input of
        <<Id:Index/binary, " -->", Post/binary>> ->
            Id2 = z_convert:to_integer(Id),
            Html = show_media_html(Id2, Context),
            process_binary_match(<<>>, Html, size(Post), show_media1(Post, 0, Context));

        <<Id:Index/binary, " {", Post/binary>> ->
            Id2 = z_convert:to_integer(Id),
            process_binary_match(<<>>, <<>>, size(Post), show_media1_opts(Id2, Post, 0, Context));
        <<_:Index/binary, _/binary>> ->
            show_media1_id(Input, Index + 1, Context);
        _ ->
            Input
    end.

% scanning for media id
show_media1_opts(Id, Input, Index, Context) ->
    case Input of
        <<Opts:Index/binary, "} -->", Post/binary>> ->
            Opts2 = mochijson:decode("{" ++ binary_to_list(Opts) ++ "}"),
            Html = show_media_html(Id, Opts2, Context),
            process_binary_match(<<>>, Html, size(Post), show_media1(Post, 0, Context));
        <<_:Index/binary, _/binary>> ->
            show_media1_opts(Id, Input, Index + 1, Context);
        _ ->
            Input
    end.



show_media_html(Id, Context) ->
    show_media_html(Id, {struct, []}, Context).

show_media_html(Id, {struct, Args}, Context) ->
	Template = "_body_media.tpl",
    Args2 = [ {list_to_atom(A), B} || {A,B} <- Args],
    z_template:render(Template, [ {id, Id} | Args2 ++ z_context:get_all(Context)  ], Context).


% Taken from quote_plus of mochiweb_util

urlencode1(Input, Index) when is_binary(Input) ->
    case Input of
        <<_:Index/binary, Byte, _/binary>> when ?NO_ENCODE(Byte) ->
            urlencode1(Input, Index + 1);
        <<Pre:Index/binary, $\s, Post/binary>> ->
            process_binary_match(Pre, <<"+">>, size(Post), urlencode1(Post, 0));
        <<Pre:Index/binary, Hi:4, Lo:4, Post/binary>> ->
            HiDigit = hexdigit(Hi),
            LoDigit = hexdigit(Lo),
            Code = <<$\%, HiDigit, LoDigit>>,
            process_binary_match(Pre, Code, size(Post), urlencode1(Post, 0));
        Input ->
            Input
    end;
urlencode1([], Acc) ->
    lists:reverse(Acc);
urlencode1([C | Rest], Acc) when ?NO_ENCODE(C) ->
    urlencode1(Rest, [C | Acc]);
urlencode1([$\s | Rest], Acc) ->
    urlencode1(Rest, [$+ | Acc]);
urlencode1([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    urlencode1(Rest, [hexdigit(Lo), hexdigit(Hi), $\% | Acc]).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

process_binary_match(Pre, Insertion, SizePost, Post) ->
    case {size(Pre), SizePost} of
        {0, 0} -> Insertion;
        {0, _} -> [Insertion, Post];
        {_, 0} -> [Pre, Insertion];
        _ -> [Pre, Insertion, Post]
    end.
