%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell
%%
%% @doc Query string processing, property lists and property maps for
%% Zotonic resources.

%% Copyright 2020 Marc Worrell
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

-module(z_props).

-include_lib("../../include/zotonic.hrl").

-export([
    from_map/1,
    from_props/1,

    from_list/1,
    from_qs/1,
    from_qs/2,

    extract_languages/1,
    prune_languages/2,

    normalize_dates/3
    ]).

%% Query String key, value and property types.
%% Used as input for the from_qs/1,2 functions.
-type qs_key() :: binary().
-type qs_value() :: binary() | #upload{} | term().
-type qs_prop() :: { qs_key(), qs_value() }.

-export_type([
    qs_key/0,
    qs_value/0,
    qs_prop/0
    ]).

%% We use -4700 as the most prehistoric date, as postgresql can't handle
%% dates before this date.
-define(EPOCH_START_YEAR, -4700).
-define(EPOCH_END_YEAR, 9999).


%% @doc Transform a map to a nested map with binary keys.
-spec from_map( map() | undefined ) -> map().
from_map(undefined) ->
    #{};
from_map(Map) ->
    maps:fold(
        fun(K, V, Acc) ->
            K1 = to_binary(K),
            Acc#{ K1 => from_any(V) }
        end,
        #{},
        Map).

from_any(M) when is_map(M) -> from_map(M);
from_any([ {_, _} | _ ] = L) -> from_props(L);
from_any(V) -> V.

%% @doc Transform a proplist from older resources and/or code to a (nested) map.
%%      This knows how to handle nestes lists like the 'blocks' list of lists.
-spec from_props( proplists:proplist() | undefined ) -> map().
from_props(undefined) ->
    #{};
from_props(Ps) when is_list(Ps) ->
    from_props_1(Ps).

from_props_1(Ps) when is_list(Ps) ->
    L1 = lists:map(fun from_prop/1, Ps),
    maps:from_list(L1);
from_props_1(P) ->
    P.

from_prop({K, [ [{_,_}|_] | _ ] = Vs}) ->
    % This is typical for the 'blocks' property, which contains
    % a list of blocks. Where each block is a property list.
    {to_binary(K), lists:map(fun from_props_1/1, Vs)};
from_prop({K, V}) ->
    from_prop_value(to_binary(K), V);
from_prop(K) when is_atom(K) ->
    {to_binary(K), true};
from_prop(K) ->
    K.

to_binary(K) when is_atom(K) -> atom_to_binary(K, utf8);
to_binary(K) when is_binary(K) -> K;
to_binary(K) -> K.

from_prop_value(K, undefined) ->
    {K, undefined};
from_prop_value(K, V) when is_boolean(V) ->
    {K, V};
from_prop_value(<<"is_", _/binary>> = K, V) ->
    {K, z_convert:to_bool(V)};
from_prop_value(K, V) when is_atom(V) ->
    {K, atom_to_binary(V, utf8)};
from_prop_value(K, "") ->
    {K, <<>>};
from_prop_value(K, [ C | _ ] = V) when is_integer(C), C >= 0, C =< 255 ->
    % Might be string with UTF8 encoded characters.
    % Can be found in legacy Zotonic 0.x code.
    try
        V1 = iolist_to_binary(V),
        {K, V1}
    catch
        error:badarg ->
            {K, V}
    end;
from_prop_value(K, #trans{ tr = Tr }) ->
    Tr1 = lists:filtermap(
        fun
            ({Iso, Text}) when is_atom(Iso), is_binary(Text) ->
                {true, {Iso, Text}};
            ({Iso, Text}) ->
                case z_language:to_language_atom(Iso) of
                    {ok, Code} ->
                        {true, {Code, z_convert:to_binary(Text)}};
                    _ ->
                        false
                end
        end,
        Tr),
    {K, #trans{ tr = Tr1 }};
from_prop_value(K, V) ->
    {K, V}.


%% @doc Convert a list for rsc insert and/or update to a map.
%%      The list could be a list of (binary) query args, or
%%      a property list with atom keys.
-spec from_list( list() ) -> {ok, #{ binary() => term() }}.
from_list([ {K, _} | _ ] = L) when is_atom(K) ->
    {ok, from_props(L)};
from_list([ {K, _} | _ ] = L) when is_binary(K) ->
    from_qs(L).


%% @doc Combine properties from a form. The form consists of a flat list
%% with query string properties.
%% The result is a map which can be used by the rsc and other routines.
%%
%% The keys can have a special format, specifying how they are processed:
%% <ul>
%%  <li> 'dt:ymd:0:property' is datetime property, 0 for start-date, 1 for end-date,
%%    the pattern 'ymd' describes what is in the value, could also be 'y', 'm',
%%    'd', 'dmy', 'his', 'hi', 'h', 'i', or 's'</li>
%%  <li> 'prop$en' is the 'en' translation of the property named 'prop'</li>
%%  <li> 'a.b.c.d' is a nested map</li>
%%  <li> 'a~1' is a multi occurence of 'a', useful for same named items</li>
%%  <li> 'blocks[].name' is a list of maps, use 'blocks[].' to append a new empty
%%    entry. Use 'name[]' to append to a list of values.</li>
%% </ul>
%%
%% If a date/time part is missing then the current UTC date is used to fill the
%% missing parts.
-spec from_qs( list( qs_prop() ) ) -> {ok, #{ binary() => term() }}.
from_qs(Qs) ->
    from_qs(Qs, calendar:universal_time()).

%% @doc Like from_qs/1, except that a specific date is used to fill in any
%       missing date/time parts.
-spec from_qs( list( qs_prop() ), calendar:datetime() ) ->
            {ok, #{ binary() => term() }}.
from_qs(Qs, Now) ->
    Nested = nested(Qs),
    WithDates = combine_dates(Nested, Now),
    WithTrans = combine_trans(WithDates),
    {ok, WithTrans}.

% -spec local_now(z:context()) -> calendar:datetime().
% local_now(Context) ->
%     z_datetime:to_local(erlang:universaltime(), Context).

% is_date_prop({<<"date_remarks", _/binary>>, _}) -> false;
% is_date_prop({<<"date_", _/binary>>, _}) -> true;
% is_date_prop({<<"publication_start">>, _}) -> true;
% is_date_prop({<<"publication_end">>, _}) -> true;
% is_date_prop({<<"org_pubdate">>, _}) -> true;
% is_date_prop(_) -> false.


%% ---------------------------------------------------------------------------------------
%% Nested maps
%% ---------------------------------------------------------------------------------------

nested(Qs) ->
    Map = lists:foldl(
        fun({K, V}, Acc) ->
            [ K1 | _ ] = binary:split(K, <<"~">>),
            Parts = binary:split(K1, <<".">>, [global]),
            nested_assign(Parts, V, Acc)
        end,
        #{},
        Qs),
    reverse_lists(Map).

reverse_lists(Map) when is_map(Map) ->
    maps:map(
        fun
            (_K, M) when is_map(M) ->
                reverse_lists(M);
            (_K, L) when is_list(L) ->
                lists:foldl(
                    fun(V, Acc) ->
                        [ reverse_lists(V) | Acc ]
                    end,
                    [],
                    L);
            (_K, V) ->
                V
        end,
        Map);
reverse_lists(V) ->
    V.

nested_assign([ <<>> | _ ], _V, Map) ->
    % Drop empty keys
    Map;
nested_assign([ <<"block-">> ], V, Map) ->
    % Handle forms with old 'block-' editing templates
    nested_assign([ <<"blocks[].">> ], V, Map);
nested_assign([ <<"block-", Rest/binary>> ], V, Map) ->
    % Handle forms with old 'block-' editing templates
    case binary:split(Rest, <<"-">>) of
        [ _ , K ] -> nested_assign([ <<"blocks[].", K/binary>> ], V, Map);
        [ K ] -> nested_assign([ <<"blocks[].", K/binary>> ], V, Map)
    end;
nested_assign([ K, <<>> ], _V, Map) ->
    % Start a new map, if key ends in "[]"
    % This was a key 'a.b[].' which notifies the
    % start of a new map.
    case has_suffix(K, <<"[]">>) of
        true ->
            Len = size(K) - size(<<"[]">>),
            <<K1:Len/binary, "[]">> = K,
            case maps:get(K1, Map, []) of
                L when is_list(L) ->
                    Map#{ K1 => [ #{} | L ] };
                _ ->
                    Map#{ K1 => [ #{} ] }
            end;
        false ->
            Map
    end;
nested_assign([ K ], V, Map) ->
    case has_suffix(K, <<"[]">>) of
        true ->
            % This was a key 'a.b[]' which appends a
            % value to a list.
            Len = size(K) - size(<<"[]">>),
            <<K1:Len/binary, "[]">> = K,
            case maps:get(K1, Map, []) of
                L when is_list(L) ->
                    Map#{ K1 => [ V | L ] };
                _ ->
                    Map#{ K1 => [ V ] }
            end;
        false ->
            Map#{ K => V }
    end;
nested_assign([ K | Ks ], V, Map) ->
    case has_suffix(K, <<"[]">>) of
        true ->
            % This was a key 'a.b[].d' which sets a
            % value in a list of maps.
            Len = size(K) - size(<<"[]">>),
            <<K1:Len/binary, "[]">> = K,
            case maps:find(K1, Map) of
                {ok, [ M | L ]} ->
                    M1 = nested_assign(Ks, V, M),
                    Map#{ K1 => [ M1 | L ]};
                _ ->
                    M1 = nested_assign(Ks, V, #{}),
                    Map#{ K1 => [ M1 ]}
            end;
        false ->
            Sub = maps:get(K, Map, #{}),
            Sub1 = nested_assign(Ks, V, Sub),
            Map#{ K => Sub1 }
    end.

%% ---------------------------------------------------------------------------------------
%% Language handling
%% ---------------------------------------------------------------------------------------

%% @doc Combine properties like 'title$en' into #trans{} records.
combine_trans(Map) when is_map(Map) ->
    MapCombined = combine_trans_1( has_trans_prop(Map), Map ),
    maps:map(
        fun
            (_K, V) when is_list(V); is_map(V) ->
                combine_trans(V);
            (_K, V) ->
                V
        end,
        MapCombined);
combine_trans(List) when is_list(List) ->
    lists:map( fun combine_trans/1, List );
combine_trans(V) ->
    V.

has_trans_prop(Map) ->
    maps:fold(
        fun
            (K, V, false) -> is_trans_prop(K, V);
            (_, _, true) -> true
        end,
        false,
        Map).

combine_trans_1(true, Map) ->
    {TransParts, OtherProps} = maps:fold(
        fun(K, V, {Ts, Os}) ->
            case is_trans_prop(K, V) of
                true ->
                    Ts1 = [ {K,V} | Ts ],
                    {Ts1, Os};
                false ->
                    Os1 = Os#{ K => V },
                    {Ts, Os1}
            end
        end,
        {[], #{}},
        Map),
    TransPartsMap = group_trans_parts(TransParts),
    maps:merge(OtherProps, TransPartsMap);
combine_trans_1(false, Map) ->
    Map.

is_trans_prop(K, V) when is_binary(V) ->
    binary:match(K, <<"$">>) =/= nomatch;
is_trans_prop(_, _) ->
    false.

group_trans_parts(TransParts) ->
    lists:foldl(
        fun
            ({K, V}, Acc) ->
                case binary:split(K, <<"$">>, [global]) of
                    [ Name, Lang ] ->
                        case z_language:to_language_atom(Lang) of
                            {ok, Code} ->
                                add_trans(Name, Code, V, Acc);
                            {error, Reason} ->
                                ?LOG_NOTICE(#{
                                    text => <<"Dropping trans part, language code is unknown">>,
                                    in => zotonic_core,
                                    result => error,
                                    reason => Reason,
                                    language => Lang,
                                    part => K
                                }),
                                Acc
                        end;
                    _ ->
                        ?LOG_NOTICE(#{
                            text => <<"Dropping unknown trans part, should be like 'title$en'">>,
                            in => zotonic_core,
                            result => error,
                            reason => format,
                            part => K
                        }),
                        Acc
                end
        end,
        #{},
        TransParts).

add_trans(Name, Code, undefined, Acc) ->
    add_trans(Name, Code, <<>>, Acc);
add_trans(Name, Code, V, Acc) ->
    #trans{ tr = Tr } = maps:get(Name, Acc, #trans{}),
    Tr1 = [ {Code, z_string:trim(V)} | proplists:delete(Code, Tr) ],
    Acc#{ Name => #trans{ tr = Tr1 } }.

%% ---------------------------------------------------------------------------------------
%% Date handling
%% ---------------------------------------------------------------------------------------

%% @doc Combine multiple qs values to single dates.
combine_dates(Map, Now) when is_map(Map) ->
    MapCombined = combine_dates_1( has_dt_prop(Map), Map, Now ),
    maps:map(
        fun
            (_K, V) when is_list(V); is_map(V) ->
                combine_dates(V, Now);
            (_K, V) ->
                V
        end,
        MapCombined);
combine_dates(List, Now) when is_list(List) ->
    lists:map( fun(V) -> combine_dates(V, Now) end, List );
combine_dates(V, _Now) ->
    V.

has_dt_prop(Map) ->
    maps:fold(
        fun
            (_, _, true) -> true;
            (<<"dt:", _/binary>>, _V, false) -> true;
            (_, _, HasDT) -> HasDT
        end,
        false,
        Map).


%% @doc Combine multiple qs values to single dates.
combine_dates_1(true, Map, Now) ->
    {DateParts, OtherProps} = maps:fold(
        fun
            (<<"dt:", _/binary>> = K, V, {Ts, Os}) ->
                Ts1 = [ {K,V} | Ts ],
                {Ts1, Os};
            (K, V, {Ts, Os}) ->
                Os1 = Os#{ K => V },
                {Ts, Os1}
        end,
        {[], #{}},
        Map),
    ByName = group_date_parts(DateParts),
    DateKVs = combine_date_parts(ByName, Now),
    DateKVs1 = cleanup_dates(DateKVs),
    maps:merge(OtherProps, maps:from_list(DateKVs1));
combine_dates_1(false, Qs, _Now) ->
    Qs.

cleanup_dates(DateKVs) ->
    lists:map(
        fun(KV) ->
            cleanup_date_prop(KV)
        end,
        DateKVs).

cleanup_date_prop({Name, Date}) ->
    {Name, cleanup_date(Date)}.

cleanup_date(<<>>) -> undefined;
cleanup_date(null) -> undefined;
cleanup_date(undefined) -> undefined;
cleanup_date(false) -> undefined;
cleanup_date({{undefined, undefined, undefined}, _}) -> undefined;
cleanup_date(DateTime) -> DateTime.

group_date_parts(DatePartsQs) ->
    lists:foldl(
        fun
            ({K, V}, Acc) ->
                case binary:split(K, <<":">>, [global]) of
                    [ <<"dt">>, Pattern, EndFlag, Name ] ->
                        IsEnd = z_convert:to_bool(EndFlag),
                        group_date_part(Name, IsEnd, Pattern, V, Acc);
                    [ K ] ->
                        group_date_part(K, false, full, V, Acc);
                    _ ->
                        ?LOG_INFO(#{
                            text => <<"Dropping unknown date part, should be like 'dt:ymd:0:propname'">>,
                            in => zotonic_core,
                            result => error,
                            reason => format,
                            part => K
                        }),
                        Acc
                end
        end,
        #{},
        DatePartsQs).

group_date_part(Name, IsEnd, Pattern, V, Acc) ->
    group_date_part_1(basename(Name, IsEnd), Pattern, V, Acc).

group_date_part_1({IsEnd, Basename, Name}, Pattern, V, Acc) ->
    % Add to accumulator, combine all parts for Basename
    Parts = maps:get(Basename, Acc, #{}),
    {_, Patterns} = maps:get(IsEnd, Parts, {Name, []}),
    Patterns1 = [ {Pattern, V} | Patterns ],
    Parts1 = Parts#{
        IsEnd => {Name, Patterns1}
    },
    Acc#{ Basename => Parts1 }.


combine_date_parts(Parts, Now) ->
    maps:fold(
        fun(_, V, Acc) ->
            Ps = combine_date_part(V, Now),
            Ps ++ Acc
        end,
        [],
        Parts).

combine_date_part(#{ false := StartParts, true := EndParts }, Now) ->
    {StartKey, StartDate} = combine_part(StartParts, false, Now),
    {EndKey, EndDate} = combine_part(EndParts, true, Now),
    StartDate1 = set_default( default_date(StartKey, Now), StartDate ),
    EndDate1 = copy_missing(EndKey, StartDate1, EndDate),
    [
        {StartKey, StartDate1},
        {EndKey, EndDate1}
    ];
combine_date_part(#{ false := StartParts }, Now) ->
    {StartKey, StartDate} = combine_part(StartParts, false, Now),
    StartDate1 = set_default( default_date(StartKey, Now), StartDate ),
    [
        {StartKey, StartDate1}
    ];
combine_date_part(#{ true := EndParts }, Now) ->
    {EndKey, EndDate} = combine_part(EndParts, true, Now),
    EndDate1 = copy_missing(EndKey, default_date(EndKey, Now), EndDate),
    [
        {EndKey, EndDate1}
    ].

combine_part({Key, Ps}, _IsEnd, Now) ->
    DateParts = lists:map(
        fun({Pattern, V}) ->
            {Pattern, to_date_value(Pattern, V)}
        end,
        Ps),
    Date = lists:foldl(
        fun
            ({_Pattern, undefined}, DateAcc) ->
                DateAcc;
            ({_Pattern, <<>>}, DateAcc) ->
                DateAcc;
            ({Pattern, DatePart}, DateAcc) ->
                merge_date_part(DateAcc, Pattern, DatePart)
        end,
        default_date(Key, Now),
        DateParts),
    {Key, Date}.


default_date(<<"date">>, _Now) -> undefined_date();
default_date(<<"date_start">>, _Now) -> undefined_date();
default_date(<<"date_end">>, _Now) -> undefined_date();
default_date(<<"publication_start">>, _Now) -> undefined_date();
default_date(<<"publication_end">>, _Now) -> ?ST_JUTTEMIS;
default_date(<<"org_pubdate">>, _Now) -> undefined_date();
default_date(_, Now) -> Now.

undefined_date() ->
    { {undefined, undefined, undefined}, {undefined, undefined, undefined} }.

merge_date_part(_Date, full, V) -> V;
merge_date_part({{_Y, M, D}, {H, I, S}}, <<"y">>, V) -> {{V, M, D}, {H, I, S}};
merge_date_part({{Y, _M, D}, {H, I, S}}, <<"m">>, V) -> {{Y, V, D}, {H, I, S}};
merge_date_part({{Y, M, _D}, {H, I, S}}, <<"d">>, V) -> {{Y, M, V}, {H, I, S}};
merge_date_part({{Y, M, D}, {_H, I, S}}, <<"h">>, V) -> {{Y, M, D}, {V, I, S}};
merge_date_part({{Y, M, D}, {H, _I, S}}, <<"i">>, V) -> {{Y, M, D}, {H, V, S}};
merge_date_part({{Y, M, D}, {H, I, _S}}, <<"s">>, V) -> {{Y, M, D}, {H, I, V}};
merge_date_part({{Y, M, D}, {_H, _I, S}}, <<"hi">>, {H, I, _S}) -> {{Y, M, D}, {H, I, S}};
merge_date_part({{Y, M, D}, _Time}, <<"his">>, {_, _, _} = V) -> {{Y, M, D}, V};
merge_date_part({_Date, {H, I, S}}, <<"ymd">>, {_, _, _} = V) -> {V, {H, I, S}};
merge_date_part({_Date, {H, I, S}}, <<"dmy">>, {_, _, _} = V) -> {V, {H, I, S}}.


to_date_value(full, V) ->
    V;
to_date_value(<<"ymd">>, <<"-", V/binary>>) ->
    case to_date_value(<<"ymd">>, V) of
        {Y, M, D} when is_integer(Y) -> {-Y, M, D};
        YMD -> YMD
    end;
to_date_value(<<"dmy">>, V) ->
    case re:run(V, "([0-9]+)[-/: ]([0-9]+)[-/: ](-?[0-9]+)", [{capture, all_but_first, binary}]) of
        nomatch -> {undefined, undefined, undefined};
        % Negative years 13/7/-99
        {match, [D, M, Y]} -> {to_int(Y), to_int(M), to_int(D)}
    end;
to_date_value(Part, V) when Part =:= <<"ymd">>; Part =:= <<"his">> ->
    case binary:split(V, [<<"-">>, <<"/">>, <<":">>, <<" ">>], [global]) of
        [<<>>] -> {undefined, undefined, undefined};
        [Y, M, D] -> {to_int(Y), to_int(M), to_int(D)}
    end;
to_date_value(<<"hi">>, V) ->
    case binary:split(V, [<<"-">>, <<"/">>, <<":">>, <<" ">>], [global]) of
        [<<>>] -> {undefined, undefined, undefined};
        [H] -> {to_int(H), 0, undefined};
        [H, I] -> {to_int(H), to_int(I), undefined}
    end;
to_date_value(_, V) ->
    to_int(V).

copy_missing( <<"publication_end">>, _S, {{undefined,undefined,undefined},{undefined,undefined,_}} ) ->
    ?ST_JUTTEMIS;
copy_missing( _Name, _S, {{undefined,undefined,undefined},{undefined,undefined,_}} ) ->
    undefined;
copy_missing( Name, {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{undefined,Me,De},{He,Ie,Se}} ) when is_integer(Ys) ->
    copy_missing( Name, {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ys,Me,De},{He,Ie,Se}} );
copy_missing( Name, {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ys,undefined,De},{He,Ie,Se}} ) when is_integer(Ms) ->
    copy_missing( Name, {{Ys,Ms,Ds},{Hs,Is,Ss}} ,{{Ys,Ms,De},{He,Ie,Se}} );
copy_missing( Name, {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ys,Ms,undefined},{He,Ie,Se}} ) when is_integer(Ds) ->
    copy_missing( Name, {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ys,Ms,Ds},{He,Ie,Se}} );
copy_missing( Name, S, {{undefined,Me,De},{He,Ie,Se}} ) ->
    copy_missing( Name, S, {{?EPOCH_END_YEAR,Me,De},{He,Ie,Se}} );
copy_missing( Name, S, {{Ye,undefined,De},{He,Ie,Se}} ) ->
    copy_missing( Name, S ,{{Ye,12,De},{He,Ie,Se}} );
copy_missing( Name, S, {{Ye,Me,undefined},{He,Ie,Se}} ) ->
    De = z_datetime:last_day_of_the_month(Ye,Me),
    copy_missing( Name, S, {{Ye,Me,De},{He,Ie,Se}} );
copy_missing( Name, S, {{Ye,Me,De},{undefined,Ie,Se}} ) ->
    copy_missing( Name, S, {{Ye,Me,De},{23,Ie,Se}} );
copy_missing( Name, S, {{Ye,Me,De},{He,undefined,Se}} ) ->
    copy_missing( Name, S, {{Ye,Me,De},{He,59,Se}} );
copy_missing( Name, S, {{Ye,Me,De},{He,Ie,undefined}} ) ->
    copy_missing( Name, S, {{Ye,Me,De},{He,Ie,59}} );
copy_missing( _Name, _S, E ) ->
    E.

set_default(Default, {{undefined, undefined, undefined}, {undefined, undefined, undefined}}) ->
    Default;
set_default(Default, {{undefined, undefined, undefined}, {0, 0, 0}}) ->
    Default;
set_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{undefined,Me,De},{He,Ie,Se}} ) when is_integer(Ys) ->
    set_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ys,Me,De},{He,Ie,Se}} );
set_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,undefined,De},{He,Ie,Se}} ) when is_integer(Ms) ->
    set_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Ms,De},{He,Ie,Se}} );
set_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,undefined},{He,Ie,Se}} ) when is_integer(Ds) ->
    set_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,Ds},{He,Ie,Se}} );
set_default( S, {{undefined,Me,De},{He,Ie,Se}} ) ->
    set_default( S, {{?EPOCH_START_YEAR,Me,De},{He,Ie,Se}} );
set_default( S, {{Ye,undefined,De},{He,Ie,Se}} ) ->
    set_default( S, {{Ye,1,De},{He,Ie,Se}} );
set_default( S, {{Ye,Me,undefined},{He,Ie,Se}} ) ->
    set_default( S, {{Ye,Me,1},{He,Ie,Se}} );
set_default( S, {{Ye,Me,De},{undefined,Ie,Se}} ) ->
    set_default( S, {{Ye,Me,De},{0,Ie,Se}} );
set_default( S, {{Ye,Me,De},{He,undefined,Se}} ) ->
    set_default( S, {{Ye,Me,De},{He,0,Se}} );
set_default( S, {{Ye,Me,De},{He,Ie,undefined}} ) ->
    set_default( S, {{Ye,Me,De},{He,Ie,0}} );
set_default( _S, {{Ye,Me,De},{He,Ie,Se}} ) ->
    {{Ye,Me,De},{He,Ie,Se}};
set_default(_Default, Dt) ->
    Dt.

basename(Name, IsEnd) ->
    case has_suffix(Name, <<"_start">>) of
        true ->
            Length = size(Name) - size(<<"_start">>),
            <<Base:Length/binary, _/binary>> = Name,
            {false, Base, Name};
        false ->
            case has_suffix(Name, <<"_end">>) of
                true ->
                    Length = size(Name) - size(<<"_end">>),
                    <<Base:Length/binary, _/binary>> = Name,
                    {true, Base, Name};
                false ->
                    {IsEnd, Name, Name}
            end
    end.


has_suffix(B, Suffix) ->
    binary:longest_common_suffix([Suffix, B]) =:= size(Suffix).

to_int(<<>>) ->
    undefined;
to_int(A) ->
    try
        binary_to_integer(A)
    catch
        _:_ -> undefined
    end.



%% @doc Find all different language codes in the maps.
-spec extract_languages( map() ) -> [ atom() ].
extract_languages( Props ) when is_map(Props) ->
    Langs = maps:fold(fun extract_languages_1/3, #{}, Props),
    lists:sort( maps:keys(Langs) ).

extract_languages_1(_, V, Langs) when is_map(V) ->
    maps:fold(fun extract_languages_1/3, Langs, V);
extract_languages_1(_, V, Langs) when is_list(V) ->
    lists:foldl(
        fun(X, Acc) ->
            extract_languages_1(k, X, Acc)
        end,
        Langs,
        V);
extract_languages_1(_, #trans{ tr = Tr }, Langs) ->
    lists:foldl(
        fun
            ({Iso, _Trans}, Acc) ->
                case maps:is_key(Iso, Acc) of
                    false -> Acc#{ Iso => true };
                    true -> Acc
                end
        end,
        Langs,
        Tr);
extract_languages_1(_, _, Langs) ->
    Langs.


%% @doc Check all trans records, remove languages not mentioned.
-spec prune_languages(map(), list(atom())) -> map().
prune_languages(Props, Langs) ->
    prune_languages_1(Props, Langs).

prune_languages_1(M, Langs) when is_map(M) ->
    maps:map(
        fun(_K, V) -> prune_languages(V, Langs) end,
        M);
prune_languages_1(L, Langs) when is_list(L) ->
    lists:map(fun(E) -> prune_languages_1(E, Langs) end, L);
prune_languages_1(#trans{ tr = Tr }, Langs) ->
    Tr1 = lists:filter(
        fun({Iso, _}) -> lists:member(Iso, Langs) end,
        Tr),
    #trans{ tr = Tr1 };
prune_languages_1(V, _Langs) ->
    V.


%% @doc Normalize dates, ensure that all dates are in UTC
%%      and parsed to Erlang datetime format.
-spec normalize_dates( m_rsc:props(), boolean(), binary()|undefined ) -> m_rsc:props().
normalize_dates(#{ <<"tz">> := Tz } = Props, IsAllDay, undefined) ->
    normalize_dates_1(Props, IsAllDay, Tz);
normalize_dates(Props, IsAllDay, Tz) ->
    normalize_dates_1(Props, IsAllDay, Tz).

normalize_dates_1(Props, IsAllDay, undefined) ->
    normalize_dates_2(Props, IsAllDay, <<"UTC">>);
normalize_dates_1(Props, IsAllDay, Tz) ->
    normalize_dates_2(Props, IsAllDay, Tz).

normalize_dates_2(M, IsAllDay, Tz) when is_map(M) ->
    maps:map(
        fun(K, V) ->
            case is_date_key(K) orelse is_date_value(V) of
                true ->
                    try
                        norm_date(K, V, IsAllDay, Tz)
                    catch
                        _:_ -> undefined
                    end;
                false ->
                    V
            end
        end,
        M);
normalize_dates_2(L, IsAllDay, Tz) when is_list(L) ->
    lists:map(
        fun(V) -> normalize_dates_2(V, IsAllDay, Tz) end,
        L);
normalize_dates_2(V, IsAllDay, Tz) ->
    case is_date_value(V) of
        true ->
            try
                norm_date(<<>>, V, IsAllDay, Tz)
            catch
                _:_ -> undefined
            end;
        false ->
            V
    end.

norm_date(_K, undefined, _IsAllDay, _Tz) ->
    undefined;
norm_date(_K, <<>>, _IsAllDay, _Tz) ->
    undefined;
norm_date(_K, V, _IsAllDay, _Tz) when is_integer(V) ->
    z_datetime:timestamp_to_datetime(V);
norm_date(K, {{Y,M,D}, {H,I,S}} = DT, true, Tz) when
    is_integer(Y), is_integer(M), is_integer(D),
    is_integer(H), is_integer(I), is_integer(S) ->
    case K of
        <<"date_start">> -> DT;
        <<"date_end">> -> DT;
        _ -> z_datetime:to_utc(DT, Tz)
    end;
norm_date(_K, {{Y,M,D}, {H,I,S}} = DT, false, Tz) when
    is_integer(Y), is_integer(M), is_integer(D),
    is_integer(H), is_integer(I), is_integer(S) ->
    z_datetime:to_utc(DT, Tz);
norm_date(K, V, true, Tz) ->
    case K of
        <<"date_start">> -> z_datetime:to_datetime(V, <<"UTC">>);
        <<"date_end">> -> z_datetime:to_datetime(V, <<"UTC">>);
        _ -> z_datetime:to_datetime(V, Tz)
    end;
norm_date(_K, V, false, Tz) ->
    z_datetime:to_datetime(V, Tz).


is_date_value({{Y,M,D}, {H,I,S}}) when
    is_integer(Y), is_integer(M), is_integer(D),
    is_integer(H), is_integer(I), is_integer(S) ->
    true;
is_date_value(_) ->
    false.

is_date_key(<<"is_", _/binary>>) -> false;
is_date_key(<<"date_is_all_day">>) -> false;
is_date_key(<<"date_remarks">>) -> false;
is_date_key(<<"date_", _/binary>>) -> true;
is_date_key(<<"org_pubdate">>) -> true;
is_date_key(<<"publication_start">>) -> true;
is_date_key(<<"publication_end">>) -> true;
is_date_key(K) when is_binary(K) ->
    case binary:longest_common_suffix([ K, <<"_date">> ]) of
        5 -> true;
        _ -> false
    end;
is_date_key(_) -> false.

