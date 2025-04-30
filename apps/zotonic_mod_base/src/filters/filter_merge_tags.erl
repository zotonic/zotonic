%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Replace tags in a text by evaluating their expressions.
%% @end

%% Copyright 2025 Marc Worrell
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

-module(filter_merge_tags).

-export([
    merge_tags/2,
    merge_tags/3,

    eval/3
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Render a template in the context of the current user.
merge_tags(Text, Context) ->
    merge_tags(Text, #{ <<"id">> => z_acl:user(Context) }, Context).

merge_tags(Text, {vars, Vars}, Context) ->
    merge_tags(Text, Vars, Context);
merge_tags(Text, Vars, Context) when is_list(Vars) ->
    merge_tags(Text, to_map(Vars), Context);
merge_tags(Text, Id, Context) when is_integer(Id) ->
    merge_tags(Text, #{ <<"id">> => Id }, Context);
merge_tags(Text, Id, Context) when is_binary(Id); is_atom(Id) ->
    merge_tags(Text, #{ <<"id">> => m_rsc:rid(Id, Context) }, Context);
merge_tags(Text, Vars, Context) ->
    Text1 = to_binary(Text, Context),
    parse(Text1, Vars, <<>>, Context).

parse(<<>>, _Vars, Acc, _Context) ->
    Acc;
parse(<<"{{%20", Rest/binary>>, Vars, Acc, Context) ->
    case binary:split(Rest, [ <<"%7D%7D">>, <<"}}">> ]) of
        [Expr, Rest1] ->
            Expr1 = z_url:url_decode(Expr),
            Result = eval(Expr1, Vars, Context),
            Result1 = z_html:escape_check(Result),
            Acc1 = <<Acc/binary, Result1/binary>>,
            parse(Rest1, Vars, Acc1, Context);
        [_] ->
            parse(Rest, Vars, <<Acc/binary, "{{">>, Context)
    end;
parse(<<"{{", Rest/binary>>, Vars, Acc, Context) ->
    case binary:split(Rest, <<"}}">>) of
        [Expr, Rest1] ->
            Expr1 = z_html:unescape(Expr),
            Result = eval(Expr1, Vars, Context),
            Result1 = z_html:escape_check(Result),
            Acc1 = <<Acc/binary, Result1/binary>>,
            parse(Rest1, Vars, Acc1, Context);
        [_] ->
            parse(Rest, Vars, <<Acc/binary, "{{">>, Context)
    end;
parse(<<"%7B%7B", Rest/binary>>, Vars, Acc, Context) ->
    case binary:split(Rest, [ <<"%7D%7D">>, <<"}}">> ]) of
        [Expr, Rest1] ->
            Expr1 = z_url:url_decode(Expr),
            Result = eval(Expr1, Vars, Context),
            Result1 = z_html:escape_check(Result),
            Acc1 = <<Acc/binary, Result1/binary>>,
            parse(Rest1, Vars, Acc1, Context);
        [_] ->
            parse(Rest, Vars, <<Acc/binary, "{{">>, Context)
    end;
parse(<<C/utf8, Rest/binary>>, Vars, Acc, Context) ->
    parse(Rest, Vars, <<Acc/binary, C/utf8>>, Context);
parse(<<_, Rest/binary>>, Vars, Acc, Context) ->
    % Drop non-utf8 data
    parse(Rest, Vars, Acc, Context).

eval(Text, Vars, Context) ->
    case z_expression:parse(Text) of
        {ok, Tree} ->
            Options = [
                {p, fun p/3},
                {filters_allowed, [
                    add_day,
                    add_month,
                    add_week,
                    add_year,
                    'after',
                    append,
                    before,
                    brlinebreaks,
                    capfirst,
                    date,
                    default,
                    eq_day,
                    first,
                    first,
                    force_escape,
                    format_duration,
                    format_integer,
                    format_number,
                    format_price,
                    format_duration,
                    'if',
                    if_defined,
                    if_none,
                    if_undefined,
                    in_future,
                    in_past,
                    insert,
                    is_list,
                    is_undefined,
                    is_visible,
                    join,
                    last,
                    length,
                    linebreaksbr,
                    lower,
                    max,
                    member,
                    min,
                    minmax,
                    round,
                    round_significant,
                    slugify,
                    striptags,
                    sub_day,
                    sub_month,
                    sub_week,
                    sub_year,
                    tail,
                    timesince,
                    to_binary,
                    to_integer,
                    to_name,
                    trim,
                    truncate,
                    truncatechars,
                    upper,
                    urlize,
                    utc,
                    yesno
                ]}
            ],
            Result = z_expression:eval(Tree, Vars, Options, Context),
            {Result1, _} = z_render:output(Result, Context),
            z_html:escape_check(iolist_to_binary(Result1));
        {error, _} ->
            <<>>
    end.

p(Id, <<"country">>, Context) ->
    p(Id, <<"address_country">>, Context);
p(Id, Country, Context) when
    Country =:= <<"address_country">>,
    Country =:= <<"mail_country">>;
    Country =:= <<"billing_country">> ->
    case m_rsc:p(Id, Country, Context) of
        undefined -> undefined;
        P -> m_l10n:country_name(P, Context)
    end;
p(Id, <<"name_full">>, Context) ->
    {Name, _} = z_template:render_to_iolist("_name.tpl", Id, Context),
    iolist_to_binary(Name);
p(Id, Prop, Context) ->
    m_rsc:p(Id, Prop, Context).


to_map(Vars) ->
    lists:foldl(
        fun
            ({K, V}, Acc) ->
                K1 = z_convert:to_binary(K),
                Acc#{ K1 => V };
            (K, Acc) ->
                K1 = z_convert:to_binary(K),
                Acc#{ K1 => true }
        end,
        #{},
        Vars).

to_binary({{_, _, _}, {_, _, _}} = DT, Context) ->
    iolist_to_binary(z_datetime:format(DT, "Y-m-d H:i", Context));
to_binary(#trans{} = Tr, Context) ->
    to_binary(z_trans:lookup_fallback(Tr, Context), Context);
to_binary(V, Context) ->
    z_convert:to_binary(V, Context).
