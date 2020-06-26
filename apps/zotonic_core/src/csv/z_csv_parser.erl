%% @doc Parse CSV file into a nested list of lines and fields.
%% @author Arjan Scherpenisse <arjan@scherpenisse.net>

%% Copyright 2010-2013 Arjan Scherpenisse
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

-module(z_csv_parser).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
    inspect_file/1,
    inspect_data/1,

    scan_lines/1,
    scan_lines/2,
    cleanup_field/1
]).

-define(CSV_CHUNK_SIZE, 32*1024).

% -type sep() :: $, | $; | $\t.
-type sep() :: 44 | 59 | 9.
-type line() :: list( binary() ).
-type lines() :: list( line() ).

-export_type([ sep/0, line/0, lines/0 ]).


-spec inspect_file( file:filename_all() ) -> {ok, line(), sep()} | {error, invalid_csv_file | term()}.
inspect_file(Filename) ->
    case file:open(Filename, [read, binary]) of
        {ok, Device} ->
            FSize = filelib:file_size(Filename),
            case file:read(Device, min(?CSV_CHUNK_SIZE, FSize)) of
                {ok, Data} ->
                    file:close(Device),
                    inspect_data(Data);
                {error, _Reason} = Error ->
                    file:close(Device),
                    Error
            end;
        {error, _Reason} = Error ->
            Error
    end.


%% @doc Check if the first row is made up of column headers.
%% The file must have at least a name and a category column.
-spec inspect_data( binary() ) -> {ok, line(), sep()} | {error, invalid_csv_file}.
inspect_data(<<>>) ->
    {error, invalid_csv_file};
inspect_data(B0) ->
    B = utf8(B0),
    case fetch_line(B) of
        {ok, Line} ->
            {ok, Tabs} = parse_line(Line, $\t),
            {ok, Comma} = parse_line(Line, $,),
            {ok, SCol} = parse_line(Line, $;),
            {_, Cols, Sep} = lists:last(lists:sort([
                                    {length(Tabs), Tabs, $\t},
                                    {length(Comma), Comma, $,},
                                    {length(SCol), SCol, $;}
                                ])),
            {ok, [ z_string:trim( z_convert:to_binary(C) ) || C <- Cols ], Sep};
        _ ->
            lager:info("Invalid CSV file, could not fetch line with column defs (is there a LF or CR at the end?)"),
            {error, invalid_csv_file}
    end.

utf8(S) ->
    case mochiutf8:valid_utf8_bytes(S) of
        S ->
            S;
        Stripped ->
            case eiconv:convert("Windows-1250", S) of
                {ok, Utf8} -> Utf8;
                {error, _} -> Stripped
            end
    end.


fetch_line(B) ->
    fetch_line(B, []).

fetch_line(<<>>, _Line) ->
    false;
fetch_line(<<10, _/binary>>, Line) ->
    {ok, lists:reverse(Line)};
fetch_line(<<13, _/binary>>, Line) ->
    {ok, lists:reverse(Line)};
fetch_line(<<C, B/binary>>, Line) ->
    fetch_line(B, [C|Line]).


%% @doc Parse a line into its columns, using a character a separator.
parse_line(Line, Sep) when is_list(Line), is_integer(Sep) ->
    parse_line(Line, Sep, [], []).

%% @doc Try to parse the line with the given field escape and quote chars.
parse_line([], _Sep, Col, Cols) ->
    {ok, lists:reverse([z_csv_parser:cleanup_field(lists:reverse(Col))|Cols])};
parse_line([Sep|Rest], Sep, Col, Cols) ->
    parse_line(Rest, Sep, [], [z_csv_parser:cleanup_field(lists:reverse(Col))|Cols]);
parse_line([C|Rest], Sep, Col, Cols) ->
    parse_line(Rest, Sep, [C|Col], Cols).


%% @doc Scan the file (or device) and return lines with fields.
-spec scan_lines( file:filename() | pid() ) -> lines().
scan_lines(DeviceOrFilename) ->
    scan_lines(DeviceOrFilename, $,).

%% @doc Scan the file (or device) and return lines with fields.
-spec scan_lines( file:filename() | pid(), sep() ) -> lines().
scan_lines(Filename, FieldSep) when is_list(Filename); is_binary(Filename) ->
    {ok, Device} = file:open(Filename, [read, binary, {encoding, latin1}]),
    Res = scan_lines(Device, FieldSep, <<>>, 0, [[]], <<>>, false),
    _ = file:close(Device),
    Res;
scan_lines(Device, FieldSep) ->
    scan_lines(Device, FieldSep, <<>>, 0, [[]], <<>>, false).

scan_lines(Device, Fs, Chunk, Index, Acc, Remainder, Quoted) ->
    case {Chunk, Quoted} of
        % Chunk is empty. Get the next chunk from the file.
        {EmptyChunk, _}
            when
                EmptyChunk =:= <<>>;
                EmptyChunk =:= <<$\\>>;
                (EmptyChunk =:= <<$">> andalso Quoted);
                EmptyChunk =:= <<13>> ->
            case io:get_chars(Device, "", ?CSV_CHUNK_SIZE) of
                eof ->
                    All = case Remainder of
                              <<>> ->
                                Acc;
                              _ ->
                                case EmptyChunk of
                                    <<$">> -> append_last_field(<<$">>, Remainder, Acc);
                                    _ -> append_last_field(<<>>, Remainder, Acc)
                                end
                          end,
                    %% Remove lastly added empty line
                    All2 = case All of
                               [[<<>>]|Rest] -> Rest;
                               [[]|Rest] -> Rest;
                               _ -> All
                           end,
                    lists:reverse(All2);
                {error, E} ->
                    throw({error, E});
                NextChunk ->
                    NewChunk = case EmptyChunk of
                                    <<>> -> NextChunk;
                                    _ -> <<EmptyChunk/binary, NextChunk/binary>>
                               end,
                    scan_lines(Device, Fs, NewChunk, 0, Acc, Remainder, Quoted)
            end;

        % Escaped characters

        {<<_Field:Index/binary, $\\, 13, 10, _Rest/binary>>, _} ->
            scan_lines(Device, Fs, Chunk, Index + 3, Acc, Remainder, Quoted);

        {<<_Field:Index/binary, $\\, _, _Rest/binary>>, _} ->
            scan_lines(Device, Fs, Chunk, Index + 2, Acc, Remainder, Quoted);

        % Quoted ----

        {<<_Field:Index/binary, $", $", _Rest/binary>>, true} ->
            scan_lines(Device, Fs, Chunk, Index + 2, Acc, Remainder, true);

        {<<_Field:Index/binary, $", _Rest/binary>>, true} ->
            scan_lines(Device, Fs, Chunk, Index + 1, Acc, Remainder, false);

        {<<_Field:Index/binary, 13, 10, _Rest/binary>>, true} ->
            scan_lines(Device, Fs, Chunk, Index + 2, Acc, Remainder, true);

        {<<_Field:Index/binary, 13, _Rest/binary>>, true} ->
            scan_lines(Device, Fs, Chunk, Index + 1, Acc, Remainder, true);

        {<<_Field:Index/binary, 10, _Rest/binary>>, true} ->
            scan_lines(Device, Fs, Chunk, Index + 1, Acc, Remainder, true);

        {<<_Field:Index/binary, _, _Rest/binary>>, true} ->
            scan_lines(Device, Fs, Chunk, Index + 1, Acc, Remainder, true);

        % Unquoted ----

        {<<_Field:Index/binary, $", _Rest/binary>>, false} when Index =:= 0 andalso Remainder =:= <<>> ->
            scan_lines(Device, Fs, Chunk, Index + 1, Acc, Remainder, true);

        {<<Field:Index/binary, 13, 10, Rest/binary>>, false} ->
            scan_lines(Device, Fs, Rest, 0, [ [] | append_last_field(Remainder, Field, Acc)], <<>>, false);

        {<<Field:Index/binary, 13, Rest/binary>>, false} ->
            scan_lines(Device, Fs, Rest, 0, [ [] | append_last_field(Remainder, Field, Acc)], <<>>, false);

        {<<Field:Index/binary, 10, Rest/binary>>, false} ->
            scan_lines(Device, Fs, Rest, 0, [ [] | append_last_field(Remainder, Field, Acc)], <<>>, false);

        {<<Field:Index/binary, Fs, Rest/binary>>, false} ->
            scan_lines(Device, Fs, Rest, 0, append_field(Remainder, Field, Acc), <<>>, false);

        {<<_Field:Index/binary, _, _Rest/binary>>, false} ->
            scan_lines(Device, Fs, Chunk, Index + 1, Acc, Remainder, false);

        % Long line; add to remainder.
        {LongLine, _} ->
            scan_lines(Device, Fs, <<>>, 0, Acc, <<Remainder/binary, LongLine/binary>>, Quoted)
    end.

append_field(<<>>, Field, [Row|Rows]) ->
    [[cleanup_field(Field)|Row]|Rows];
append_field(Prefix, Field, [Row|Rows]) ->
    NewField = <<Prefix/binary, Field/binary>>,
    [[cleanup_field(NewField)|Row]|Rows].
append_last_field(Prefix, Field, Acc) ->
    [R|RS] = append_field(Prefix, Field, Acc),
    [lists:reverse(R)|RS].


%% Remove any quotes and whitespace around the fields.
cleanup_field(L) when is_list(L) ->
    cleanup_field(z_convert:to_binary(L));
cleanup_field(<<>>) ->
    <<>>;
cleanup_field(<<$", _/binary>> = S) ->
    utf8(unescape(z_convert:to_binary(z_string:trim(z_string:unquote(S))), true));
cleanup_field(S) ->
    utf8(unescape(z_convert:to_binary(z_string:trim(S)), false)).


unescape(S, IsQuoted) ->
    unescape(S, <<>>, IsQuoted).

unescape(<<>>, Acc, _IsQuoted) ->
    Acc;
unescape(<<$\\, $\\, Rest/binary>>, Acc, IsQuoted) ->
    unescape(Rest, <<Acc/binary, $\\>>, IsQuoted);
unescape(<<$\\, $n, Rest/binary>>, Acc, IsQuoted) ->
    unescape(Rest, <<Acc/binary, 10>>, IsQuoted);
unescape(<<$\\, $r, Rest/binary>>, Acc, IsQuoted) ->
    unescape(Rest, <<Acc/binary, 13>>, IsQuoted);
unescape(<<$\\, $t, Rest/binary>>, Acc, IsQuoted) ->
    unescape(Rest, <<Acc/binary, 9>>, IsQuoted);
unescape(<<$\\, $', Rest/binary>>, Acc, IsQuoted) ->
    unescape(Rest, <<Acc/binary, $'>>, IsQuoted);
unescape(<<$\\, $", Rest/binary>>, Acc, IsQuoted) ->
    unescape(Rest, <<Acc/binary, $">>, IsQuoted);
unescape(<<$", $", Rest/binary>>, Acc, true) ->
    unescape(Rest, <<Acc/binary, $">>, true);
unescape(<<C, Rest/binary>>, Acc, IsQuoted) ->
    unescape(Rest, <<Acc/binary, C>>, IsQuoted).
