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

-module(parse_csv).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
    scan_lines/1,
    scan_lines/2,
    cleanup_field/1
]).

-define(CHUNK_SIZE, 4096).


scan_lines(Device) ->
    scan_lines(Device, $,).

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
            case io:get_chars(Device, "", ?CHUNK_SIZE) of
                eof ->
                    All = case Remainder of 
                              <<>> -> 
                                Acc;
                              _ ->
                                case EmptyChunk of
                                    <<$">> -> append_field(<<$">>, Remainder, Acc);
                                    _ -> append_field(<<>>, Remainder, Acc)
                                end
                          end,
                    %% Remove lastly added empty line
                    All2 = case All of
                               [[<<>>]|Rest] -> Rest;
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
            lager:warning("Escaped NL"),
            scan_lines(Device, Fs, Chunk, Index + 3, Acc, Remainder, Quoted);

        {<<_Field:Index/binary, $\\, _, _Rest/binary>>, _} ->
            lager:warning("Escaped Char"),
            scan_lines(Device, Fs, Chunk, Index + 2, Acc, Remainder, Quoted);

        % Quoted ----

        {<<_Field:Index/binary, $", $", _Rest/binary>>, true} ->
            scan_lines(Device, Fs, Chunk, Index + 2, Acc, Remainder, true);

        {<<_Field:Index/binary, $", _Rest/binary>>, true} ->
            scan_lines(Device, Fs, Chunk, Index + 1, Acc, Remainder, false);

        {<<_Field:Index/binary, 13, 10, _Rest/binary>>, true} ->
            scan_lines(Device, Fs, Chunk, Index + 2, Acc, Remainder, true);

        {<<_Field:Index/binary, 10, _Rest/binary>>, true} ->
            scan_lines(Device, Fs, Chunk, Index + 1, Acc, Remainder, true);

        {<<_Field:Index/binary, _, _Rest/binary>>, true} ->
            scan_lines(Device, Fs, Chunk, Index + 1, Acc, Remainder, true);

        % Unquoted ----

        {<<_Field:Index/binary, $", _Rest/binary>>, false} when Index =:= 0 andalso Remainder =:= <<>> ->
            scan_lines(Device, Fs, Chunk, Index + 1, Acc, Remainder, true);

        {<<Field:Index/binary, 13, 10, Rest/binary>>, false} ->
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


