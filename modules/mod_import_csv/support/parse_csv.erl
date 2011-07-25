%% @doc Import a csv file according to the derived file/record definitions.
%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @date 2010-06-26

-module(parse_csv).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
    scan_lines/2,
    trim_field/1
]).

%% @doc Scan all lines, split in columns.
%% @todo Handle quoted fields (with escapes).
scan_lines(Device, FieldSep) ->
    scan_lines(Device, FieldSep, <<>>, 0, [[]], <<>>).


-define(CHUNK_SIZE, 4096).

scan_lines(Device, Fs, Chunk, Index, Acc, Remainder) ->
    case Chunk of
        <<>> ->
            %%
            %% Chunk is empty. Get the next chunk from the file.
            %%
            case io:get_chars(Device, "", ?CHUNK_SIZE) of
                eof ->
                    All = case Remainder of 
                              <<>> ->
                                  Acc;
                              _ ->
                                  append_field(<<>>, Remainder, Acc)
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
                    scan_lines(Device, Fs, NextChunk, 0, Acc, Remainder)
            end;

        <<Field:Index/binary, C, Rest/binary>> ->
            case C of
                13 -> scan_lines(Device, Fs, Rest, 0, [ [] | append_last_field(Remainder, Field, Acc)], <<>>);
                10 -> scan_lines(Device, Fs, Rest, 0, [ [] | append_last_field(Remainder, Field, Acc)], <<>>);
                Fs -> scan_lines(Device, Fs, Rest, 0, append_field(Remainder, Field, Acc), <<>>);
                _ -> scan_lines(Device, Fs, Chunk, Index + 1, Acc, Remainder)
            end;
        LongLine ->
            %% Long line; add to remainder.
            scan_lines(Device, Fs, <<>>, 0, Acc, <<Remainder/binary, LongLine/binary>>)
    end.

append_field(<<>>, Field, [Row|Rows]) ->
    [[trim_field(Field)|Row]|Rows];
append_field(Prefix, Field, [Row|Rows]) ->
    NewField = <<Prefix/binary, Field/binary>>,
    [[trim_field(NewField)|Row]|Rows].
append_last_field(Prefix, Field, Acc) ->
    [R|RS] = append_field(Prefix, Field, Acc),
    [lists:reverse(R)|RS].

%% Remove any quotes and whitespace around the fields.
trim_field(S) ->
    z_string:trim(z_string:unquote(S)).

