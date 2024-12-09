%% @doc Write data to a CSV file
%% @author Marc Worrell <marc@worrell.nl>

%% Copyright 2013-2019 Marc Worrell
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

-module(z_csv_writer).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    sanitize/2,

    write_file/2,
    write_file/3,

    encode_line/2
    ]).

-spec sanitize( file:filename_all(), file:filename_all() ) -> ok.
sanitize(InFile, OutFile) ->
    {ok, _Hs, Sep} = z_csv_parser:inspect_file(InFile),
    Lines = z_csv_parser:scan_lines(InFile, Sep),
    ok = z_csv_writer:write_file(OutFile, Lines, Sep).


-spec write_file( file:filename_all(), z_csv_parser:lines() ) -> ok | {error, term()}.
write_file(Filename, Data) ->
    write_file(Filename, Data, $,).

-spec write_file( file:filename_all(), z_csv_parser:lines(), z_csv_parser:sep() ) -> ok | {error, term()}.
write_file(Filename, Data, Sep) when is_list(Data) ->
    case file:open(Filename, [write, binary]) of
        {ok, Device} ->
            lists:map(
                fun(Line) ->
                    Bytes = encode_line(Line, Sep),
                    file:write(Device, Bytes)
                end,
                Data),
            file:close(Device),
            ok;
        {error, _} = Error ->
            Error
    end.

encode_line([], _Sep) ->
    <<"\r\n">>;
encode_line([V], _Sep) ->
    iolist_to_binary([ encode_value(V), <<"\r\n">> ]);
encode_line([V|Xs], Sep) ->
    iolist_to_binary([
        encode_value(V),
        [ [Sep, encode_value(X)] || X <- Xs ],
        <<"\r\n">>
        ]).

encode_value(<<>>) ->
    <<>>;
encode_value(B) when is_binary(B) ->
    quote( escape( field(B) ) );
encode_value(V) ->
    encode_value(z_convert:to_binary(V)).

quote(B) -> <<$", B/binary, $">>.

escape(B) -> escape(B, <<>>).
escape(<<>>, Acc) -> Acc;
escape(<<$", B/binary>>, Acc) -> escape(B, <<Acc/binary, $", $">>);
escape(<<10, B/binary>>, Acc) -> escape(B, <<Acc/binary, 10>>);
escape(<<13, 10, B/binary>>, Acc) -> escape(B, <<Acc/binary, 13, 10>>);
escape(<<13, B/binary>>, Acc) -> escape(B, <<Acc/binary, 13>>);
escape(<<X, B/binary>>, Acc) when X < 32 -> escape(B, Acc);
escape(<<X, B/binary>>, Acc) -> escape(B, <<Acc/binary, X>>).


%% @doc Quote all (non numerical) values that start with =, -, +, or @.
%%      Also quote values starting with whitespace, as the whitespace might
%%      be stripped by the CSV importer of office applications.
%%      See https://www.owasp.org/index.php/CSV_Injection
field(<<>>) -> <<>>;
field(<<$', _/binary>> = D) -> D;
field(<<WS, _/binary>> = D) when WS =< 32 -> <<$', D/binary>>;
field(<<$=, _/binary>> = D) -> <<$', D/binary>>;
field(<<$@, _/binary>> = D) -> <<$', D/binary>>;
field(<<C, _/binary>> = D) when C =:= $-; C =:= $+ ->
    case is_valid_number(D) of
        true -> D;
        false -> <<$', D/binary>>
    end;
field(D) -> D.


is_valid_number(D) ->
    re:run(D, <<"^[\\+\\-]?[0-9]+(\\.[0-9]+)?([eE][\\+\\-]?[0-9]+)?$">>) =/= nomatch.

