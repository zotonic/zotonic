%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Tests for iCalendar parsing, event validation, discovery and native resource links.
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

-module(import_feed_ical_tests).
-include_lib("eunit/include/eunit.hrl").

calendar(Events) -> iolist_to_binary([
    "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//Test//EN\r\n", Events, "END:VCALENDAR\r\n"]).
event(Fields) -> iolist_to_binary([
    "BEGIN:VEVENT\r\nUID:example-1\r\nDTSTAMP:20261024T120000Z\r\n", Fields, "END:VEVENT\r\n"]).
parse(Events) -> z_import_feed_parse:parse(calendar(Events), <<"https://example.org/calendar.ics">>).

normalization_test() ->
    {ok, #{entries := [E]}} = parse(event([
        "DTSTART:20261101T100000Z\r\nDTEND:20261101T110000Z\r\n",
        "SUMMARY:Coffee\\, tea\\; more\r\nDESCRIPTION:Line one\\nLine two\r\n",
        "LOCATION:Room 1\r\nURL:https://example.org/event\r\n",
        "BEGIN:VALARM\r\nDESCRIPTION:Ignore alarm\r\nEND:VALARM\r\n"])),
    ?assertEqual({{2026,11,1},{10,0,0}}, maps:get(date_start,E)),
    ?assertEqual({{2026,11,1},{11,0,0}}, maps:get(date_end,E)),
    ?assertEqual(<<"Coffee, tea; more">>, maps:get(title,E)),
    ?assertEqual(<<"Line one\nLine two">>, maps:get(body,E)),
    ?assertEqual(<<"Room 1">>, maps:get(location,E)).

all_day_and_duration_test() ->
    {ok, #{entries := [Day, Days, Timed]}} = parse([
        event("DTSTART;VALUE=DATE:20261101\r\n"),
        event("DTSTART;VALUE=DATE:20261101\r\nDTEND;VALUE=DATE:20261104\r\n"),
        event("DTSTART:20261101T100000Z\r\nDURATION:PT1H30M\r\n")]),
    ?assertEqual(true, maps:get(date_is_all_day,Day)),
    ?assertEqual({{2026,11,1},{0,0,0}}, maps:get(date_end,Day)),
    ?assertEqual({{2026,11,3},{0,0,0}}, maps:get(date_end,Days)),
    ?assertEqual({{2026,11,1},{11,30,0}}, maps:get(date_end,Timed)).

invalid_events_test() ->
    Bad = ["DTSTART:20260230T100000Z\r\n", "DTSTART:20261101T250000Z\r\n",
        "DTSTART:20261101T100000Z\r\nDTEND:20261101T100000Z\r\n",
        "DTSTART:20261101T100000Z\r\nDTEND;VALUE=DATE:20261102\r\n",
        "DTSTART:20261101T100000Z\r\nDTEND:20261101T110000Z\r\nDURATION:PT1H\r\n",
        "DTSTART:20261101T100000Z\r\nUID:duplicate\r\n"],
    {ok, #{entries := []}} = parse([event(F) || F <- Bad]),
    {ok, #{entries := []}} = parse(<<"BEGIN:VEVENT\r\nUID:missing-stamp\r\nDTSTART:20261101T100000Z\r\nEND:VEVENT\r\n">>),
    ?assertEqual({error, invalid_feed}, z_import_feed_parse:parse(<<"BEGIN:VCALENDAR\nBEGIN:VEVENT\n">>, <<"https://example.org/">>)).

native_and_paging_test() ->
    {ok, #{entries := [E], next_url := Next, is_zotonic_feed := true}} = parse([
        "X-ZOTONIC-NEXT:/calendar.ics?page=2\r\n",
        event("DTSTART:20261101T100000Z\r\nX-ZOTONIC-RESOURCE:/api/model/rsc_export/get/42\r\n")]),
    ?assertEqual(<<"https://example.org/calendar.ics?page=2">>, Next),
    ?assertEqual(<<"https://example.org/api/model/rsc_export/get/42">>, maps:get(zotonic_export,E)),
    Options = z_import_feed_native:options(#{is_subscribe => true, media_mode => <<"none">>}, #{}),
    ?assertEqual(true, proplists:get_value(is_subscribe,Options)).

folding_and_limit_test() ->
    Evt = event("DTSTART:20261101T100000Z\r\nSUMMARY:Long\r\n  title\r\n"),
    {ok, #{entries := [E | _] = Entries, truncated := true}} = parse(lists:duplicate(101,Evt)),
    ?assertEqual(100,length(Entries)),
    ?assertEqual(<<"Long title">>,maps:get(title,E)),
    {ok, Links} = z_import_feed_parse:discover(<<"<html><link rel='alternate' type='text/calendar' href='/events.ics'></html>">>, <<"https://example.org/">>),
    ?assertEqual([<<"https://example.org/events.ics">>], [maps:get(url, Link) || Link <- Links]).

timezones_and_recurrence_test() ->
    {ok, _} = application:ensure_all_started(qdate),
    {ok, #{entries := [E, Override]}} = parse([
        event("DTSTART;TZID=Europe/Amsterdam:20260701T100000\r\nRRULE:FREQ=WEEKLY\r\n"),
        event("DTSTART:20260708T090000Z\r\nRECURRENCE-ID:20260708T080000Z\r\n")]),
    ?assertEqual({{2026,7,1},{8,0,0}}, maps:get(date_start,E)),
    ?assertEqual(<<"FREQ=WEEKLY">>, maps:get(rrule,E)),
    ?assertNotEqual(maps:get(uri,E), maps:get(uri,Override)).

unsafe_links_test() ->
    {ok, #{entries := [E]}} = parse(event(
        "DTSTART:20261101T100000Z\r\nURL:javascript:alert(1)\r\nX-ZOTONIC-RESOURCE:file:///etc/passwd\r\n")),
    ?assertEqual(<<>>, maps:get(url,E)),
    ?assertEqual(undefined, maps:get(zotonic_export,E)).
