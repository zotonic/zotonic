%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2022 Marc Worrell
%% @doc Utility functions for datetime handling and representation.

%% Copyright 2009-2022 Marc Worrell
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

-module(z_datetime).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    format/2,
    format/3,

    format_utc/2,
    format_utc/3,

    to_local/2,
    to_utc/2,

    to_datetime/1,
    to_datetime/2,

    timesince/2,
    timesince/3,
    timesince/4,
    timesince/5,

    week_start/0,
    week_start/2,

    days_in_year/1,

    prev_year/1,
    prev_year/2,
    prev_month/1,
    prev_month/2,
    prev_week/1,
    prev_week/2,
    prev_day/1,
    prev_day/2,
    prev_hour/1,
    prev_hour/2,
    prev_minute/1,
    prev_minute/2,
    prev_second/1,
    prev_second/2,

    next_year/1,
    next_year/2,
    next_month/1,
    next_month/2,
    next_week/1,
    next_week/2,
    next_day/1,
    next_day/2,
    next_hour/1,
    next_hour/2,
    next_minute/1,
    next_minute/2,
    next_second/1,
    next_second/2,

    diff/2,

    month_boundaries/1,
    week_boundaries/1,
    week_boundaries/2,

    timestamp/0,
    msec/0,

    timestamp_to_datetime/1,
    datetime_to_timestamp/1,

    undefined_if_invalid_date/1,
    maybe_fix_datetime/1,

    last_day_of_the_month/2,
    is_leap_year/1
]).


-include_lib("zotonic.hrl").


% A date, same as the calendar:datetime() but with a larger year range.
-type datetime() :: calendar:datetime()
                  | {{integer(),1..12,1..31}, {0..23,0..59,0..59}}.

-type date() :: calendar:date()
              | {integer(),1..12,1..31}.

% Data that looks like a datetime and can be coerced into a datetime.
-type fixable_datetime() :: datetime()
                          | date()
                          | {integer(),integer(),integer()}
                          | {{integer(),integer(),integer()}, {integer()|undefined,integer()|undefined,integer()|undefined}}.

-type timestamp() :: integer().

-export_type([ datetime/0, date/0, fixable_datetime/0, timestamp/0 ]).


%% @doc Format the current date according to the format and the timezone settings in the context.
format(Format, Context) ->
    format(calendar:universal_time(), Format, Context).

%% @doc Format a date according to the format and the timezone settings in the context.
format(Date, Format, #context{} = Context) ->
    z_dateformat:format(z_datetime:to_local(Date, Context), Format, format_opts(Date, z_context:tz(Context), Context)).

%% @doc Format the current date in UTC.
format_utc(Format, Context) ->
    format_utc(calendar:universal_time(), Format, Context).

%% @doc Format the date using the UTC timezone.
format_utc(Date, Format, #context{} = Context) ->
    z_dateformat:format(Date, Format, format_opts(Date, "GMT", Context)).

format_opts(Date, Tz, Context) ->
    [
        {utc, Date},
        {tz, z_convert:to_list(Tz)},
        {tr, {l10n_date, [Context]}}
    ].

%% @doc Convert a time to the local context time using the current timezone.
-spec to_local(DateTime, TimeZone) -> calendar:datetime() | undefined when
    DateTime :: calendar:datetime() | undefined | time_not_exists,
    TimeZone :: string() | binary() | z:context().
to_local(undefined, _Tz) ->
    undefined;
to_local(time_not_exists, _Tz) ->
    undefined;
to_local({_Y, _M, _D} = Date, Tz) ->
    to_local({Date, {0,0,0}}, Tz);
to_local({{9999, _, _}, _} = DT, _Tz) ->
    DT;
to_local({{Y, _, _}, _}, _Tz) when Y > 9999 ->
    ?ST_JUTTEMIS;
to_local({{Y, M, D}, T}, Tz) when Y =< 1 ->
    {{Y1, M1, D1}, T1} = to_local({{10, M, D}, T}, Tz),
    {{Y1 - 10 + Y, M1, D1}, T1};
to_local(DT, <<"UTC">>) ->
    DT;
to_local(DT, <<"GMT">>) ->
    DT;
to_local(DT, <<>>) ->
    DT;
to_local(DT, #context{} = Context) ->
    to_local(DT, z_context:tz(Context));
to_local(DT, Tz) ->
    try
        case qdate:to_date(z_convert:to_list(Tz), {DT, "GMT"}) of
            {ambiguous, _Standard, Daylight} ->
                Daylight;
            {{_, _, _}, {_, _, _}} = NewDT ->
                NewDT
        end
    catch
        Type:Reason ->
            ?LOG_WARNING(#{
                text => <<"Error converting date for to_local">>,
                in => zotonic_core,
                tz => Tz,
                date => DT,
                result => Type,
                reason => Reason
            }),
            undefined
    end.

%% @doc Convert a time to the local context time using the current timezone.
-spec to_utc(DateTime, TimeZone) -> calendar:datetime() | undefined when
    DateTime :: calendar:datetime() | undefined | time_not_exists,
    TimeZone :: string() | binary() | z:context().
to_utc(undefined, _Tz) ->
    undefined;
to_utc(time_not_exists, _Tz) ->
    undefined;
to_utc({_Y, _M, _D} = Date, Tz) ->
    to_utc({Date, {0,0,0}}, Tz);
to_utc({{9999, _, _}, _} = DT, _Tz) ->
    DT;
to_utc({{Y, _, _}, _}, _Tz) when Y > 9999 ->
    ?ST_JUTTEMIS;
to_utc({{Y, M, D}, T}, Tz) when Y =< 1 ->
    {{Y1, M1, D1}, T1} = to_utc({{10, M, D}, T}, Tz),
    {{Y1 - 10 + Y, M1, D1}, T1};
to_utc(DT, <<"UTC">>) ->
    DT;
to_utc(DT, <<"GMT">>) ->
    DT;
to_utc(DT, <<>>) ->
    DT;
to_utc(DT, #context{} = Context) ->
    to_utc(DT, z_context:tz(Context));
to_utc(DT, Tz) ->
    try
        case qdate:to_date("GMT", {DT, z_convert:to_list(Tz)}) of
            {ambiguous, _Standard, Daylight} ->
                Daylight;
            {{_, _, _}, {_, _, _}} = NewDT ->
                NewDT
        end
    catch
        Type:Reason ->
            ?LOG_WARNING(#{
                text => <<"Error converting date for to_utc">>,
                in => zotonic_core,
                tz => Tz,
                date => DT,
                result => Type,
                reason => Reason
            }),
            undefined
    end.


%% @doc Convert an input to a (universal) datetime, using to_date/1 and
%% to_time/1.  When the input is a string, it is expected to be in iso
%% 8601 format, although it can also handle timestamps without time
%% zones. The time component of the datetime is optional.
-spec to_datetime( Input ) -> calendar:datetime() | undefined when
    Input :: undefined
            | binary()
            | string()
            | integer()
            | calendar:datetime()
            | {Y::integer(), M::pos_integer(), D::pos_integer()}.
to_datetime(undefined) ->
    undefined;
to_datetime(N) when is_integer(N) ->
    z_datetime:timestamp_to_datetime(N);
to_datetime(B) when is_binary(B); is_list(B) ->
    case z_utils:only_digits(B) of
        true ->
            to_datetime(z_convert:to_integer(B));
        false ->
            to_dt(B, calendar:universal_time())
    end;
to_datetime(DT) ->
    to_dt(DT, calendar:universal_time()).

to_datetime(DT, Tz) ->
    Now = to_local(calendar:universal_time(), Tz),
    to_utc(to_dt(DT, Now), Tz).

to_dt({{_,_,_},{_,_,_}} = DT, _Now) -> DT;
to_dt({_,_,_} = D, _Now) -> {D, {0,0,0}};
to_dt(<<"now">>, Now) -> Now;
to_dt(<<"today">>, Now) -> Now;
to_dt(<<"tomorrow">>, Now) -> relative_time(1, '+', [<<"day">>], Now);
to_dt(<<"yesterday">>, Now) -> relative_time(1, '+', [<<"day">>], Now);
to_dt(<<"+", Relative/binary>>, Now) -> to_relative_time('+', Relative, Now);
to_dt(<<"-", Relative/binary>>, Now) -> to_relative_time('-', Relative, Now);
to_dt("now", Now) -> Now;
to_dt("today", Now) -> Now;
to_dt("tomorrow", Now) -> relative_time(1, '+', [<<"day">>], Now);
to_dt("yesterday", Now) -> relative_time(1, '+', [<<"day">>], Now);
to_dt("+"++Relative, Now) -> to_relative_time('+', Relative, Now);
to_dt("-"++Relative, Now) -> to_relative_time('-', Relative, Now);
to_dt(DT, _Now) -> z_convert:to_datetime(DT).

to_relative_time(Op, S, Now) when is_list(S) ->
    to_relative_time(Op, z_convert:to_binary(S), Now);
to_relative_time(Op, S, Now) when is_binary(S) ->
    Ts = binary:split(S, <<" ">>, [ global, trim_all ]),
    relative_time(1, Op, Ts, Now).

relative_time(_N, Op, [<<C, _/binary>>=N|Ts], Now) when C >= $0, C =< $9 ->
    relative_time(binary_to_integer(N), Op, Ts, Now);
relative_time(N, '+', [<<"minute", _/binary>>|_], Now) ->    next_minute(Now, N);
relative_time(N, '+', [<<"hour", _/binary>>|_], Now) ->      next_hour(Now, N);
relative_time(N, '+', [<<"day", _/binary>>|_], Now) ->       next_day(Now, N);
relative_time(N, '+', [<<"sunday", _/binary>>|_], Now) ->    next_day(week_start(7, Now), N*7);
relative_time(N, '+', [<<"monday", _/binary>>|_], Now) ->    next_day(week_start(1, Now), N*7);
relative_time(N, '+', [<<"tuesday", _/binary>>|_], Now) ->   next_day(week_start(2, Now), N*7);
relative_time(N, '+', [<<"wednesday", _/binary>>|_], Now) -> next_day(week_start(3, Now), N*7);
relative_time(N, '+', [<<"thursday", _/binary>>|_], Now) ->  next_day(week_start(4, Now), N*7);
relative_time(N, '+', [<<"friday", _/binary>>|_], Now) ->    next_day(week_start(5, Now), N*7);
relative_time(N, '+', [<<"saturday", _/binary>>|_], Now) ->  next_day(week_start(6, Now), N*7);
relative_time(N, '+', [<<"week", _/binary>>|_], Now) ->      next_week(Now, N);
relative_time(N, '+', [<<"month", _/binary>>|_], Now) ->     next_month(Now, N);
relative_time(N, '+', [<<"year", _/binary>>|_], Now) ->      next_year(Now, N);
relative_time(N, '-', [<<"day", _/binary>>|_], Now) ->       prev_day(Now, N);
relative_time(N, '-', [<<"sunday", _/binary>>|_], Now) ->    prev_day(week_start(7, Now), N*7);
relative_time(N, '-', [<<"monday", _/binary>>|_], Now) ->    prev_day(week_start(1, Now), N*7);
relative_time(N, '-', [<<"tuesday", _/binary>>|_], Now) ->   prev_day(week_start(2, Now), N*7);
relative_time(N, '-', [<<"wednesday", _/binary>>|_], Now) -> prev_day(week_start(3, Now), N*7);
relative_time(N, '-', [<<"thursday", _/binary>>|_], Now) ->  prev_day(week_start(4, Now), N*7);
relative_time(N, '-', [<<"friday", _/binary>>|_], Now) ->    prev_day(week_start(5, Now), N*7);
relative_time(N, '-', [<<"week", _/binary>>|_], Now) ->      prev_week(Now, N);
relative_time(N, '-', [<<"month", _/binary>>|_], Now) ->     prev_month(Now, N);
relative_time(N, '-', [<<"year", _/binary>>|_], Now) ->      prev_year(Now, N);
relative_time(_N, _Op, _Unit, _Now) ->         undefined.

%% @doc Show a humanized version of a relative datetime.  Like "4 months, 3 days ago".
%% @spec timesince(Date, Context) -> string()
timesince(Date, Context) ->
    timesince(Date, calendar:universal_time(), Context).

%% @doc Show a humanized version of a period between two dates.  Like "4 months, 3 days ago".
%% @spec timesince(Date, BaseDate, Context) -> string()
timesince(Date, Base, Context) ->
    timesince(Date, Base, ?__(<<"ago">>, Context), ?__(<<"now">>, Context), ?__(<<"in">>, Context), 2, Context).

timesince(Date, Base, IndicatorStrings, Context) ->
    timesince(Date, Base, IndicatorStrings, 2, Context).

%% @spec timesince(Date, BaseDate, IndicatorStrings, Mode, Context) -> string()
%% @doc Show a humanized version of a period between two dates.  Like "4 months, 3 days ago".
%% `WhenText' is a string containing a maximum of three tokens. Example "ago, now, in"
timesince(Date, Base, IndicatorStrings, Mode, Context) ->
    %% strip the tokens, so the user can specify the text more flexible.
    case [string:strip(S, both) || S <- string:tokens(z_convert:to_list(IndicatorStrings), ",")] of
    [AgoText, NowText, InText] ->
        timesince(Date, Base, AgoText, NowText, InText, Mode, Context);
    [AgoText, NowText] ->
        timesince(Date, Base, AgoText, NowText, "", Mode, Context);
    [AgoText] ->
        timesince(Date, Base, AgoText, "", "", Mode, Context);
    [] ->
        timesince(Date, Base, "", "", "", Mode, Context)
    end.

%% @doc Show a humanized version of a period between two dates.  Like "4 months, 3 days ago".
%% @spec timesince(Date, BaseDate, AgoText, NowText, InText, Mode, Context) -> string()
timesince(undefined, _, _AgoText, _NowText, _InText, _Mode, _Context) ->
    "";
timesince(_, undefined, _AgoText, _NowText, _InText, _Mode, _Context) ->
    "";
timesince(Date, Base, _AgoText, NowText, _InText, _Mode, _Context) when Date == Base ->
    NowText;
timesince(Date, Base, _AgoText, _NowText, InText, Mode, Context) when Date > Base ->
    combine({InText, combine(reldate(Base, Date, Mode, Context))}, " ");
timesince(Date, Base, AgoText, _NowText, _InText, Mode, Context) ->
    combine({combine(reldate(Date, Base, Mode, Context)), AgoText}, " ").

combine(Tup) -> combine(Tup, ", ").

combine({"", B},   _Sep) -> B;
combine({A,""},    _Sep) -> A;
combine({<<>>, B}, _Sep) -> B;
combine({A,<<>>},  _Sep) -> A;
combine({A,B}, Sep) -> [A,Sep,B].



%% @doc Return a string describing the relative date difference.
reldate(D1, D2, 1, Context) ->
    {A, _} = reldate(D1,D2, 2, Context),
    {A,[]};
reldate(D1, D2, 2, Context) ->
    case diff(D1,D2) of
        {{0,0,0},{0,0,0}} -> {?__(<<"now">>,Context),[]};
        {{0,0,0},{0,0,S}} when S < 10 -> {?__(<<"moments">>,Context),[]};
        {{0,0,0},{0,0,S}} -> {plural(S, ?__(<<"second">>,Context), ?__(<<"seconds">>,Context)),
                              []};
        {{0,0,0},{0,I,S}} -> {plural(I, ?__(<<"minute">>,Context), ?__(<<"minutes">>,Context)),
                              plural(S, ?__(<<"second">>,Context), ?__(<<"seconds">>,Context))};
        {{0,0,0},{H,I,_}} -> {plural(H, ?__(<<"hour">>,Context),   ?__(<<"hours">>,Context)),
                              plural(I, ?__(<<"minute">>,Context), ?__(<<"minutes">>,Context))};
        {{0,0,D},{H,_,_}} -> {plural(D, ?__(<<"day">>,Context),    ?__(<<"days">>,Context)),
                              plural(H, ?__(<<"hour">>,Context),   ?__(<<"hours">>,Context))};
        {{0,M,D},{_,_,_}} -> {plural(M, ?__(<<"month">>,Context),  ?__(<<"months">>,Context)),
                              plural(D, ?__(<<"day">>,Context),    ?__(<<"days">>,Context))};
        {{Y,M,_},{_,_,_}} -> {plural(Y, ?__(<<"year">>,Context),   ?__(<<"years">>,Context)),
                              plural(M, ?__(<<"month">>,Context),  ?__(<<"months">>,Context))}
    end.


plural(0,_Single,_Plural) ->
    "";
plural(1,Single,_Plural) ->
    [$1, 32, Single];
plural(N,_Single,Plural) ->
    [integer_to_list(N), 32, Plural].


%% @doc Return the date the current week starts (monday)
week_start() ->
    week_start(1, calendar:universal_time()).

week_start(StartDayNr, {D,_}) ->
    Today = {D,{0,0,0}},
    WeekDay = calendar:day_of_the_week(D),
    if
        WeekDay > StartDayNr -> prev_day(Today, WeekDay - StartDayNr);
        WeekDay =:= StartDayNr -> Today;
        WeekDay < StartDayNr -> prev_day(Today, WeekDay - StartDayNr + 7)
    end.

%% @doc Return the date one year earlier.
prev_year({{Y,M,D},T}) ->
    norm_month({{Y-1,M,D}, T});
prev_year({_,_,_} = Date) ->
    prev_year({Date, {0,0,0}}).

prev_year({{Y,M,D}, T}, N) ->
    DT1 = {{Y-N,M,D}, T},
    norm_month(DT1);
prev_year({_,_,_} = Date, N) ->
    prev_year({Date, {0,0,0}}, N).

%% @doc Return the date one month earlier.
prev_month(DT) -> next_month(DT, -1).
prev_month(DT, N) -> next_month(DT, -N).

%% @doc Return the date one week earlier.
prev_week(DT) ->
    prev_week_1(DT, 7).

prev_week_1(DT, 0) -> DT;
prev_week_1(DT, N) ->
    DT1 = prev_day(DT),
    prev_week_1(DT1, N-1).

prev_week(Date, N) ->
    nth(Date, -N, fun next_week/1, fun prev_week/1).

%% @doc Return the date one day earlier.
prev_day({{_,_,1},_} = Date) ->
    {{Y1,M1,_},T1} = prev_month(Date),
    {{Y1,M1,last_day_of_the_month(Y1,M1)}, T1};
prev_day({{Y,M,D},T}) ->
    {{Y,M,D-1}, T};
prev_day({_,_,_} = Date) ->
    prev_day({Date, {0,0,0}}).

prev_day(Date, N) ->
    nth(Date, -N, fun next_day/1, fun prev_day/1).

%% @doc Return the date one hour earlier.
prev_hour({_,{0,_,_}} = Date) ->
    {YMD,{_,I,S}} = prev_day(Date),
    {YMD,{23,I,S}};
prev_hour({YMD,{H,I,S}}) ->
    {YMD, {H-1,I,S}}.

prev_hour(Date, N) ->
    nth(Date, -N, fun next_hour/1, fun prev_hour/1).

%% @doc Return the date one minute earlier.
prev_minute({_,{_,0,_}} = Date) ->
    {YMD,{H,_,S}} = prev_hour(Date),
    {YMD,{H,59,S}};
prev_minute({YMD,{H,I,S}}) ->
    {YMD, {H,I-1,S}}.

prev_minute(Date, N) ->
    nth(Date, -N, fun next_minute/1, fun prev_minute/1).

%% @doc Return the date one second earlier.
prev_second({_,{_,_,0}} = Date) ->
    {YMD,{H,I,_}} = prev_minute(Date),
    {YMD,{H,I,59}};
prev_second({YMD,{H,I,S}}) ->
    {YMD, {H,I,S-1}}.

prev_second(Date, N) ->
    nth(Date, -N, fun next_second/1, fun prev_second/1).

%% @doc Return the date one year later.
next_year({{Y,M,D},T}) ->
    norm_month({{Y+1,M,D}, T});
next_year({_,_,_} = Date) ->
    next_year({Date, {0,0,0}}).

next_year({{Y,M,D}, T}, N) ->
    DT1 = {{Y+N,M,D}, T},
    norm_month(DT1);
next_year({_,_,_} = Date, N) ->
    next_year({Date, {0,0,0}}, N).

%% @doc Return the date one month later. Gives unpredictable results if the
%%      day doesn't exist in the next month. (eg. feb 30 will become feb 28).
next_month(DT) ->
    next_month(DT, 1).

next_month({{Y, M, D}, T}, N) ->
    DT1 = {{Y, M+N, D}, T},
    norm_month(DT1);
next_month({_,_,_} = Date, N) ->
    next_month({Date, {0,0,0}}, N).

%% @doc Move the date so that the month/day number is valid.
norm_month({{Y, M, D}, T}) when M =< 0 ->
    norm_month({{Y-1, M+12, D}, T});
norm_month({{Y, M, D}, T}) when M > 12 ->
    norm_month({{Y+1, M-12, D}, T});
norm_month({{Y, M, D}, T}) ->
    D1 = erlang:min(last_day_of_the_month(Y,M), D),
    {{Y, M, D1}, T}.

%% @doc Return the date one week later.
next_week(DT) ->
    next_week_1(DT, 7).

next_week_1(DT, 0) ->
    DT;
next_week_1(DT, N) ->
    DT1 = next_day(DT),
    next_week_1(DT1, N-1).

next_week(Date, N) ->
    nth(Date, N, fun next_week/1, fun prev_week/1).

%% @doc Return the date one day later.
next_day({{Y,M,D},T} = Date) ->
    case last_day_of_the_month(Y,M) of
        D1 when D1 =< D ->
            {{Y1,M1,_},T1} = next_month(Date),
            {{Y1,M1,1},T1};
        _ ->
            {{Y,M,D+1},T}
    end;
next_day({_,_,_} = Date) ->
    next_day({Date, {0,0,0}}).

next_day(Date, N) ->
    nth(Date, N, fun next_day/1, fun prev_day/1).

%% @doc Return the date one hour later.
next_hour({_,{23,_,_}} = Date) ->
    {YMD,{_,I,S}} = next_day(Date),
    {YMD,{0,I,S}};
next_hour({YMD,{H,I,S}}) ->
    {YMD, {H+1,I,S}}.

next_hour(Date, N) ->
    nth(Date, N, fun next_hour/1, fun prev_hour/1).

%% @doc Return the date one minute later.
next_minute({_,{_,59,_}} = Date) ->
    {YMD,{H,_,S}} = next_hour(Date),
    {YMD,{H,0,S}};
next_minute({YMD,{H,I,S}}) ->
    {YMD, {H,I+1,S}}.

next_minute(Date, N) ->
    nth(Date, N, fun next_minute/1, fun prev_minute/1).

%% @doc Return the date one second later.
next_second({_,{_,_,59}} = Date) ->
    {YMD,{H,I,_}} = next_minute(Date),
    {YMD,{H,I,0}};
next_second({YMD,{H,I,S}}) ->
    {YMD, {H,I,S+1}}.

next_second(Date, N) ->
    nth(Date, N, fun next_second/1, fun prev_second/1).

nth(Date, 0, _Next, _Prev) ->
    Date;
nth(Date, N, Next, Prev) when is_integer(N), N > 0 ->
    nth(Next(Date), N-1, Next, Prev);
nth(Date, N, Next, Prev) when is_integer(N), N < 0 ->
    nth(Prev(Date), N+1, Next, Prev).

%% @doc Return the number of days in a certain year.
days_in_year(Y) ->
    case calendar:is_leap_year(Y) of
        true -> 366;
        false -> 365
    end.

%% @doc Return the absolute difference between two dates.  Does not take daylight saving into account.
diff({Y,M,D}, Date2) when is_integer(Y), is_integer(M), is_integer(D) ->
    diff({{Y,M,D},{0,0,0}}, Date2);
diff(Date1, {Y,M,D}) when is_integer(Y), is_integer(M), is_integer(D) ->
    diff(Date1, {{Y,M,D},{0,0,0}});
diff(Date1, Date2) when Date1 < Date2 ->
    diff(Date2,Date1);
diff({YMD1,{H1,I1,S1}}, {_,{_,_,S2}} = Date2) when S2 > S1 ->
    NextDate2 = next_minute(Date2),
    diff({YMD1,{H1,I1,S1+60}},NextDate2);
diff({YMD1,{H1,I1,S1}}, {_,{_,I2,_}} = Date2) when I2 > I1 ->
    NextDate2 = next_hour(Date2),
    diff({YMD1,{H1,I1+60,S1}},NextDate2);
diff({YMD1,{H1,I1,S1}}, {_,{H2,_,_}} = Date2) when H2 > H1 ->
    NextDate2 = next_day(Date2),
    diff({YMD1,{H1+24,I1,S1}},NextDate2);
diff({{Y1,M1,D1},T1}, {{_Y2,_M2,D2},_} = Date2) when D2 > D1 ->
    NextDate2 = next_day(Date2),
    diff({{Y1,M1,D1+1},T1},NextDate2);
diff({{Y1,M1,D1},T1}, {{_,M2,_},_} = Date2) when M2 > M1 ->
    NextDate2 = next_year(Date2),
    diff({{Y1,M1+12,D1},T1},NextDate2);

diff({{Y1,M1,D1},{H1,I1,S1}}, {{Y2,M2,D2},{H2,I2,S2}}) ->
    {{Y1-Y2, M1-M2, D1-D2}, {H1-H2, I1-I2, S1-S2}}.


%% @doc Return the month-boundaries of a given date
month_boundaries({{Y,M,_}, _}) ->
    Start = {{Y,M,1}, {0,0,0}},
    {End,_} = prev_day(next_month(Start)),
    {Start, {End, {23,59,59}}}.


%% @doc Return the week-boundaries of a given date.
%% WeekStart is optional, and determines on which day a week starts.
week_boundaries(Date) ->
    week_boundaries(Date, 1).

week_boundaries({D,_T}=Date, WeekStart) ->
    DOW = calendar:day_of_the_week(D),
    Start = -weeknorm(DOW - WeekStart),
    {S,_} = day_add(Date, Start),
    {E,_} = day_add(Date, Start + 6),
    { {S, {0,0,0}}, {E, {23,59,59}} }.

weeknorm(D) when D < 0 ->
    weeknorm(D+7);
weeknorm(D) when D > 6 ->
    weeknorm(D-7);
weeknorm(D) ->
    D.

day_add(Date, 0) ->
    Date;
day_add(Date, Num) when Num < 0 ->
    day_add(prev_day(Date), Num + 1);
day_add(Date, Num) when Num > 0 ->
    day_add(next_day(Date), Num - 1).


%% @doc Return the millisec value of the current clock.
-spec msec() -> integer().
msec() ->
    erlang:system_time(millisecond).

% Constant value of calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(SECS_1970, 62167219200).

%% @doc Calculate the current UNIX timestamp (seconds since Jan 1, 1970)
-spec timestamp() -> SecondsSince1970
    when SecondsSince1970 :: integer().
timestamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time())-?SECS_1970.

%% @doc Translate UNIX timestamp to local datetime.
timestamp_to_datetime(Seconds) ->
   calendar:gregorian_seconds_to_datetime(?SECS_1970 + Seconds).

%% @doc Translate a local time date to UNIX timestamp
datetime_to_timestamp(?ST_JUTTEMIS) ->
    undefined;
datetime_to_timestamp(undefined) ->
    undefined;
datetime_to_timestamp(DT) ->
    calendar:datetime_to_gregorian_seconds(DT) - ?SECS_1970.

%% @doc Return 'undefined' if a given date is invalid
undefined_if_invalid_date({{Y,M,D},{H,I,S}} = Date) when
    is_integer(Y), is_integer(M), is_integer(D),
    is_integer(H), is_integer(I), is_integer(S),
    H >= 0, H =< 23, I >= 0, I =< 59, S >= 0, S =< 59,
    M >= 1, M =< 12, D >= 1, Y >= -4713, Y =< 9999
    ->
        MaxDays = case M of
                    1 -> 31;
                    3 -> 31;
                    5 -> 31;
                    7 -> 31;
                    8 -> 31;
                    10 -> 31;
                    12 -> 31;
                    2 ->
                        case Y rem 400 of
                            0 -> 29;
                            _ ->
                                case Y rem 100 of
                                    0 -> 28;
                                    _ ->
                                        case Y rem 4 of
                                            0 -> 29;
                                            _ -> 28
                                        end
                                end
                        end;
                    _ ->
                        30
                  end,
        case D =< MaxDays of
            true -> Date;
            false -> undefined
        end;
undefined_if_invalid_date(_) ->
    undefined.


%% @doc Shift a date if the date falls outside the valid date or time ranges. Return undefined
%% if the date could not be mapped to some valid date.
-spec maybe_fix_datetime(fixable_datetime() | undefined) -> datetime() | undefined.
maybe_fix_datetime({_,_,_} = Date) ->
    maybe_fix_datetime({Date, {0,0,0}});
maybe_fix_datetime({{Y,M,D},{H,I,undefined}}) ->
    maybe_fix_datetime({{Y,M,D},{H,I,0}});
maybe_fix_datetime({{Y,M,D},{H,undefined,S}}) ->
    maybe_fix_datetime({{Y,M,D},{H,0,S}});
maybe_fix_datetime({{Y,M,D},{undefined,I,S}}) ->
    maybe_fix_datetime({{Y,M,D},{0,I,S}});
maybe_fix_datetime({{Y,M,D},{H,I,S}}) when
    not is_integer(Y); not is_integer(M); not is_integer(D),
    not is_integer(H); not is_integer(I); not is_integer(S) ->
    undefined;
maybe_fix_datetime({{Y,M,D},{H,I,S}}) when is_integer(H), H < 0 ->
    maybe_fix_datetime(prev_day({{Y,M,D},{H+24,I,S}}));
maybe_fix_datetime({{Y,M,D},{H,I,S}}) when is_integer(I), I < 0 ->
    maybe_fix_datetime(prev_hour({{Y,M,D},{H,I+60,S}}));
maybe_fix_datetime({{Y,M,D},{H,I,S}}) when is_integer(S), S < 0 ->
    maybe_fix_datetime(prev_minute({{Y,M,D},{H,I,S+60}}));
maybe_fix_datetime({{Y,M,D},{H,I,S}}) when is_integer(H), H >= 24 ->
    maybe_fix_datetime(next_day({{Y,M,D},{H-24,I,S}}));
maybe_fix_datetime({{Y,M,D},{H,I,S}}) when is_integer(I), I >= 60 ->
    maybe_fix_datetime(next_hour({{Y,M,D},{H,I-60,S}}));
maybe_fix_datetime({{Y,M,D},{H,I,S}}) when is_integer(S), S >= 60 ->
    maybe_fix_datetime(next_minute({{Y,M,D},{H,I,S-60}}));
maybe_fix_datetime({{Y,M,D},{H,I,S}}) when is_integer(M), M =< 0 ->
    maybe_fix_datetime(prev_month({{Y,1,D},{H,I,S}}, 0-M+1));
maybe_fix_datetime({{Y,M,D},{H,I,S}}) when is_integer(D), D =< 0 ->
    maybe_fix_datetime(prev_day({{Y,M,1},{H,I,S}}, 0-D+1));
maybe_fix_datetime({{Y,M,D},{H,I,S}} = Date) when
    is_integer(Y), is_integer(M), is_integer(D),
    is_integer(H), is_integer(I), is_integer(S) ->
    norm_month(Date);
maybe_fix_datetime(_) ->
    undefined.



%% Routines below are adapted from calendar.erl, which is:
%%
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
%%
%% The adaptation is to make the routines work for BCE.

-type year() :: integer().
-type month() :: 1..12.
-type ldom() :: 28 | 29 | 30 | 31.

%% last_day_of_the_month(Year, Month)
%%
%% Returns the number of days in a month.
%%
-spec last_day_of_the_month(Year, Month) -> LastDay when
      Year :: year(),
      Month :: month(),
      LastDay :: ldom().
last_day_of_the_month(Y, M) when is_integer(Y), is_integer(M) ->
    last_day_of_the_month1(Y, M).

-spec last_day_of_the_month1(year(),month()) -> ldom().
last_day_of_the_month1(_, 4) -> 30;
last_day_of_the_month1(_, 6) -> 30;
last_day_of_the_month1(_, 9) -> 30;
last_day_of_the_month1(_,11) -> 30;
last_day_of_the_month1(Y, 2) ->
   case is_leap_year(Y) of
      true -> 29;
      _    -> 28
   end;
last_day_of_the_month1(_, M) when is_integer(M), M > 0, M < 13 ->
    31.

%% is_leap_year(Year) = true | false
%%
-spec is_leap_year(Year) -> boolean() when
      Year :: year().
is_leap_year(Year) when Year rem 4 =:= 0, Year rem 100 =/= 0 ->
    true;
is_leap_year(Year) when Year rem 400 =:= 0 ->
    true;
is_leap_year(_) ->
    false.
