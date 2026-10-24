%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Normalize iCalendar VEVENTs into the shared feed import representation.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_import_feed_ical).
-export([parse/3]).

%% @doc Parse a calendar already bounded by z_import_feed_parse:parse/2's byte
%% limit. Url is the base for relative links; Limit bounds the VEVENTs examined,
%% including invalid events. Returns normalized feed metadata and entries, or
%% {error, invalid_feed} for malformed calendar structure. Invalid individual
%% events are omitted. Names stay binaries and no external resources are fetched.
%%
%% Events require UID, UTC DTSTAMP and DTSTART. All-day DTEND is converted from
%% an exclusive calendar end to Zotonic's inclusive end date. Known IANA TZIDs
%% are converted to UTC; floating times use UTC. Custom VTIMEZONE definitions
%% and alarms are ignored. Recurrence rules are retained without expansion;
%% RECURRENCE-ID distinguishes exceptions from the master event.
%%
%% X-ZOTONIC-RESOURCE selects native resource import (and its optional WebSub
%% subscriptions). X-ZOTONIC-NEXT supplies pagination. Both links pass through
%% the shared HTTP URL resolver. Cancelled events are marked for unpublication
%% by the ordinary feed importer. Unknown or invalid dates skip the event.
-spec parse(binary(), binary(), pos_integer()) -> {ok, map()} | {error, term()}.
parse(Data, Url, Limit) ->
    try
        Unfolded = re:replace(Data, <<"\\r?\\n[ \\t]">>, <<>>, [global, {return, binary}]),
        Lines = [L || L <- binary:split(binary:replace(Unfolded, <<"\r\n">>, <<"\n">>, [global]), <<"\n">>, [global]), L =/= <<>>],
        {Calendar, Events} = components(Lines, [], [], [], []),
        <<"2.0">> = value(<<"VERSION">>, Calendar),
        true = value(<<"PRODID">>, Calendar) =/= <<>>,
        Entries = lists:filtermap(fun(E) -> entry(E, Url) end, lists:sublist(Events, Limit)),
        {ok, #{url => Url, title => text(value(<<"X-WR-CALNAME">>, Calendar)),
            website => Url, next_url => resolve(value(<<"X-ZOTONIC-NEXT">>, Calendar), Url),
            hubs => [], refresh_seconds => 0, entries => Entries,
            is_zotonic_feed => lists:any(fun(E) -> is_binary(maps:get(zotonic_export, E)) end, Entries),
            truncated => length(Events) > Limit}}
    catch _:_ -> {error, invalid_feed} end.

%% Only properties directly inside VEVENT belong to the event. Ignore nested
%% VALARM/VTIMEZONE contents, while requiring balanced component boundaries.
components([], [], Calendar, [], Events) -> {Calendar, lists:reverse(Events)};
components([<<"BEGIN:VCALENDAR">> | Rest], [], [], [], []) ->
    components(Rest, [<<"VCALENDAR">>], [], [], []);
components([<<"END:VCALENDAR">>], [<<"VCALENDAR">>], Calendar, [], Events) ->
    components([], [], Calendar, [], Events);
components([<<"BEGIN:VEVENT">> | Rest], [<<"VCALENDAR">>] = Stack, C, [], Es) ->
    components(Rest, [<<"VEVENT">> | Stack], C, [], Es);
components([<<"END:VEVENT">> | Rest], [<<"VEVENT">>, <<"VCALENDAR">>], C, E, Es) ->
    components(Rest, [<<"VCALENDAR">>], C, [], [lists:reverse(E) | Es]);
components([<<"BEGIN:", Name/binary>> | Rest], [_ | _] = Stack, C, E, Es) ->
    true = Name =/= <<"VEVENT">> andalso Name =/= <<"VCALENDAR">>,
    components(Rest, [Name | Stack], C, E, Es);
components([<<"END:", Name/binary>> | Rest], [Name | Stack], C, E, Es) ->
    components(Rest, Stack, C, E, Es);
components([Line | Rest], [<<"VCALENDAR">>] = Stack, C, E, Es) ->
    components(Rest, Stack, [property(Line) | C], E, Es);
components([Line | Rest], [<<"VEVENT">>, <<"VCALENDAR">>] = Stack, C, E, Es) ->
    components(Rest, Stack, C, [property(Line) | E], Es);
components([_ | Rest], [_, _ | _] = Stack, C, E, Es) ->
    components(Rest, Stack, C, E, Es).

property(Line) ->
    %% Colons and semicolons inside quoted parameters are not delimiters.
    [Head, Value] = split(Line, $:, false, <<>>, [], 1),
    [Name | Params] = split(Head, $;, false, <<>>, [], infinity),
    {string:uppercase(Name), maps:from_list([parameter(P) || P <- Params]), Value}.
parameter(P) ->
    [K, V] = binary:split(P, <<"=">>),
    {string:uppercase(K), string:trim(V, both, [$"])}.
split(<<>>, _, false, Acc, Parts, _) -> lists:reverse([Acc | Parts]);
split(<<$", R/binary>>, Sep, Quote, Acc, Parts, N) ->
    split(R, Sep, not Quote, <<Acc/binary, $">>, Parts, N);
split(<<Sep, R/binary>>, Sep, false, Acc, Parts, 1) -> lists:reverse([R, Acc | Parts]);
split(<<Sep, R/binary>>, Sep, false, Acc, Parts, infinity) ->
    split(R, Sep, false, <<>>, [Acc | Parts], infinity);
split(<<C, R/binary>>, Sep, Q, Acc, Parts, N) -> split(R, Sep, Q, <<Acc/binary, C>>, Parts, N).

value(Name, Props) ->
    case lists:keyfind(Name, 1, Props) of {_, _, V} -> V; false -> <<>> end.
required(Name, Props) ->
    [P] = [P || {K, _, _} = P <- Props, K =:= Name],
    P.

entry(Props, Url) ->
    try
        %% Reject duplicate singleton fields and inconsistent event intervals.
        lists:foreach(fun(K) -> true = length([ok || {N, _, _} <- Props, N =:= K]) =< 1 end,
            [<<"UID">>, <<"DTSTAMP">>, <<"DTSTART">>, <<"DTEND">>, <<"DURATION">>,
             <<"SUMMARY">>, <<"DESCRIPTION">>, <<"URL">>, <<"RECURRENCE-ID">>, <<"RRULE">>,
             <<"CREATED">>, <<"LAST-MODIFIED">>, <<"STATUS">>, <<"LOCATION">>]),
        {_, _, UID} = required(<<"UID">>, Props),
        true = UID =/= <<>>,
        Stamp = utc(required(<<"DTSTAMP">>, Props)),
        {Start, AllDay} = datetime(required(<<"DTSTART">>, Props)),
        End = event_end(Start, AllDay, Props),
        Recurrence = value(<<"RECURRENCE-ID">>, Props),
        case Recurrence of
            <<>> -> ok;
            _ -> {_, AllDay} = datetime(required(<<"RECURRENCE-ID">>, Props))
        end,
        Guid = case Recurrence of <<>> -> UID; _ -> <<UID/binary, "#", Recurrence/binary>> end,
        Link = resolve(value(<<"URL">>, Props), Url),
        Native = resolve(value(<<"X-ZOTONIC-RESOURCE">>, Props), Url),
        {true, #{uri => z_import_feed_parse:identity(Guid, Link, Url), guid => Guid,
            url => Link, title => z_html:escape(text(value(<<"SUMMARY">>, Props))),
            summary => <<>>, body => z_html:escape(text(value(<<"DESCRIPTION">>, Props))),
            author => text(value(<<"ORGANIZER">>, Props)),
            published => optional_utc(<<"CREATED">>, Props, undefined),
            modified => optional_utc(<<"LAST-MODIFIED">>, Props, Stamp),
            date_start => Start, date_end => End, date_is_all_day => AllDay,
            is_cancelled => value(<<"STATUS">>, Props) =:= <<"CANCELLED">>,
            location => text(value(<<"LOCATION">>, Props)),
            rrule => value(<<"RRULE">>, Props), recurrence_id => Recurrence,
            tags => [], media => [], media_aliases => #{},
            zotonic_export => case Native of <<>> -> undefined; _ -> Native end,
            raw => Props}}
    catch _:_ -> false end.

optional_utc(Name, Props, Default) ->
    case lists:keyfind(Name, 1, Props) of false -> Default; P -> utc(P) end.
utc({_, Params, <<_:15/binary, "Z">>} = P) ->
    false = maps:is_key(<<"TZID">>, Params),
    {DT, false} = datetime(P), DT.

datetime({_, Params, <<Y:4/binary, M:2/binary, D:2/binary>>}) ->
    <<"DATE">> = maps:get(<<"VALUE">>, Params, <<"DATE-TIME">>),
    false = maps:is_key(<<"TZID">>, Params),
    {valid_date(Y, M, D, <<"00">>, <<"00">>, <<"00">>), true};
datetime({_, Params, <<Y:4/binary, M:2/binary, D:2/binary, "T", H:2/binary, I:2/binary, S:2/binary, Suffix/binary>>}) ->
    <<"DATE-TIME">> = maps:get(<<"VALUE">>, Params, <<"DATE-TIME">>),
    DT = valid_date(Y, M, D, H, I, S),
    TZ = case Suffix of
        <<"Z">> -> false = maps:is_key(<<"TZID">>, Params), <<"UTC">>;
        <<>> -> maps:get(<<"TZID">>, Params, <<"UTC">>)
    end,
    {{_, _, _}, {_, _, _}} = UTC = z_datetime:to_utc(DT, TZ),
    {UTC, false}.
valid_date(Y, M, D, H, I, S) ->
    [YY, MM, DD, HH, II, SS] = [binary_to_integer(V) || V <- [Y, M, D, H, I, S]],
    true = YY > 0 andalso calendar:valid_date({YY, MM, DD}),
    true = HH >= 0 andalso HH < 24 andalso II >= 0 andalso II < 60 andalso SS >= 0 andalso SS < 60,
    {{YY, MM, DD}, {HH, II, SS}}.

event_end(Start, AllDay, Props) ->
    End = case {lists:keyfind(<<"DTEND">>, 1, Props), value(<<"DURATION">>, Props)} of
        {false, <<>>} when AllDay -> add_seconds(Start, 86400);
        {false, <<>>} -> Start;
        {false, Duration} ->
            Seconds = duration(Duration),
            true = not AllDay orelse Seconds rem 86400 =:= 0,
            add_seconds(Start, Seconds);
        {P, <<>>} ->
            {DT, AllDay} = datetime(P),
            true = DT > Start,
            DT
    end,
    %% Zotonic uses an inclusive end date for all-day events.
    case AllDay of true -> add_seconds(End, -86400); false -> End end.
add_seconds(DT, N) -> calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(DT) + N).
duration(Value) ->
    {match, Parts} = re:run(Value, <<"^\\+?P(?:(\\d+)W|(\\d+)D(?:T(?:(\\d+)H)?(?:(\\d+)M)?(?:(\\d+)S)?)?|T(?:(\\d+)H)?(?:(\\d+)M)?(?:(\\d+)S)?)$">>, [{capture, all_but_first, binary}]),
    N = lists:sum([case V of <<>> -> 0; _ -> binary_to_integer(V) * Factor end
        || {V, Factor} <- lists:zip(Parts ++ lists:duplicate(8 - length(Parts), <<>>), [604800,86400,3600,60,1,3600,60,1])]),
    true = N > 0, N.

text(Value) -> iolist_to_binary(unescape(Value)).
unescape(<<$\\, C, Rest/binary>>) when C =:= $n; C =:= $N -> [<<"\n">> | unescape(Rest)];
unescape(<<$\\, C, Rest/binary>>) -> [<<C>> | unescape(Rest)];
unescape(<<C, Rest/binary>>) -> [<<C>> | unescape(Rest)];
unescape(<<>>) -> [].
resolve(Value, Url) -> z_import_feed_http:resolve(Value, Url).
