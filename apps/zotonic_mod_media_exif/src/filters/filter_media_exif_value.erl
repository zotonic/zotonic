%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Format EXIF values for display in templates.
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

-module(filter_media_exif_value).
-moduledoc("
Formats EXIF values for display in templates.

The EXIF parser returns Erlang values such as `{ratio, N, D}` tuples, lists of
ratios for GPS coordinates, and date strings in EXIF format. This filter
converts those values to compact human-readable text. The optional argument is
the EXIF field name and enables field-specific formatting:

```django
{{ value|media_exif_value:name|escape }}
```
").

-export([
    media_exif_value/2,
    media_exif_value/3
]).


-spec media_exif_value(Value, Context) -> iodata()
    when
        Value :: term(),
        Context :: z:context().
media_exif_value(Value, Context) ->
    media_exif_value(Value, undefined, Context).


-spec media_exif_value(Value, Name, Context) -> iodata()
    when
        Value :: term(),
        Name :: term(),
        Context :: z:context().
media_exif_value(undefined, _Name, _Context) ->
    <<>>;
media_exif_value(<<>>, _Name, _Context) ->
    <<>>;
media_exif_value(Value, Name, _Context) ->
    format_value(normalize_name(Name), Value).


format_value(Name, Value) when Name =:= <<"gps_latitude">>; Name =:= <<"gps_longitude">> ->
    format_gps(Value);
format_value(<<"gps_altitude">>, {ratio, N, D}) ->
    [ format_decimal(ratio_to_float(N, D), 1), " m" ];
format_value(<<"gps_altitude_ref">>, 0) ->
    <<"Above sea level">>;
format_value(<<"gps_altitude_ref">>, 1) ->
    <<"Below sea level">>;
format_value(Name, Value)
    when Name =:= <<"gps_dest_bearing">>;
         Name =:= <<"gps_img_direction">> ->
    format_degrees(Value);
format_value(Name, Value)
    when Name =:= <<"gps_dest_bearing_ref">>;
         Name =:= <<"gps_img_direction_ref">> ->
    format_direction_ref(Value);
format_value(<<"gps_latitude_ref">>, Value) ->
    format_latitude_ref(Value);
format_value(<<"gps_longitude_ref">>, Value) ->
    format_longitude_ref(Value);
format_value(<<"gps_speed">>, {ratio, N, D}) ->
    format_decimal(ratio_to_float(N, D), 1);
format_value(<<"gps_speed_ref">>, Value) ->
    format_speed_ref(Value);
format_value(<<"gps_date_stamp">>, Value) ->
    format_exif_date(Value);
format_value(<<"gps_time_stamp">>, Value) ->
    format_gps_time(Value);
format_value(<<"exposure_time">>, {ratio, N, D}) ->
    format_exposure_time(N, D);
format_value(<<"shutter_speed_value">>, {ratio, N, D}) ->
    format_exposure_seconds(1 / math:pow(2, ratio_to_float(N, D)));
format_value(<<"f_number">>, {ratio, N, D}) ->
    [ "f/", format_decimal(ratio_to_float(N, D), 1) ];
format_value(<<"aperture_value">>, {ratio, N, D}) ->
    [ "f/", format_decimal(math:pow(math:sqrt(2), ratio_to_float(N, D)), 1) ];
format_value(<<"max_aperture_value">>, {ratio, N, D}) ->
    [ "f/", format_decimal(math:pow(math:sqrt(2), ratio_to_float(N, D)), 1) ];
format_value(<<"focal_length">>, {ratio, N, D}) ->
    [ format_decimal(ratio_to_float(N, D), 1), " mm" ];
format_value(<<"components_configuration">>, Value) ->
    format_components(Value);
format_value(<<"subject_area">>, Value) ->
    format_subject_area(Value);
format_value(Name, Value)
    when Name =:= <<"date_time">>;
         Name =:= <<"date_time_original">>;
         Name =:= <<"date_time_digitized">> ->
    format_exif_date(Value);
format_value(Name, Value)
    when Name =:= <<"subsec_time">>;
         Name =:= <<"subsec_time_original">>;
         Name =:= <<"subsec_time_digitized">> ->
    format_ascii(Value);
format_value(_Name, Value) ->
    format_any(Value).


normalize_name(undefined) ->
    undefined;
normalize_name(Name) when is_binary(Name) ->
    Name;
normalize_name(Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
normalize_name(Name) ->
    z_convert:to_binary(Name).


format_any({ratio, N, D}) ->
    format_ratio(N, D);
format_any(L) when is_list(L), L =:= [] ->
    <<>>;
format_any(L) when is_list(L) ->
    case maybe_string(L) of
        B when is_binary(B) ->
            B;
        false ->
            join([ format_any(V) || V <- L ], <<", ">>)
    end;
format_any(B) when is_binary(B) ->
    case maybe_printable_binary(B) of
        true -> B;
        false -> format_binary_bytes(B)
    end;
format_any({{_Y, _M, _D}, {_H, _I, _S}} = Date) ->
    format_date(Date);
format_any(N) when is_integer(N) ->
    integer_to_binary(N);
format_any(N) when is_float(N) ->
    format_decimal(N, 2);
format_any(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
format_any(Value) ->
    io_lib:format("~p", [Value]).


format_gps([{ratio, D, DF}, {ratio, M, MF}, {ratio, S, SF}]) ->
    [
        format_decimal(ratio_to_float(D, DF), 0),
        "˚ ",
        format_decimal(ratio_to_float(M, MF), 0),
        "' ",
        format_decimal(ratio_to_float(S, SF), 2),
        "\""
    ];
format_gps(Value) ->
    format_any(Value).


format_degrees({ratio, N, D}) ->
    [ format_decimal(ratio_to_float(N, D), 2), "˚" ];
format_degrees(Value) when is_number(Value) ->
    [ format_decimal(Value, 2), "˚" ];
format_degrees(Value) ->
    format_any(Value).


format_direction_ref(Value) ->
    case ascii_first(Value) of
        $T -> <<"True north">>;
        $M -> <<"Magnetic north">>;
        _ -> format_ascii(Value)
    end.


format_latitude_ref(Value) ->
    case ascii_first(Value) of
        $N -> <<"North">>;
        $S -> <<"South">>;
        _ -> format_ascii(Value)
    end.


format_longitude_ref(Value) ->
    case ascii_first(Value) of
        $E -> <<"East">>;
        $W -> <<"West">>;
        _ -> format_ascii(Value)
    end.


format_speed_ref(Value) ->
    case ascii_first(Value) of
        $K -> <<"km/h">>;
        $M -> <<"mph">>;
        $N -> <<"knots">>;
        _ -> format_ascii(Value)
    end.


format_gps_time([{ratio, H, HF}, {ratio, M, MF}, {ratio, S, SF}]) ->
    [
        two_digits(round(ratio_to_float(H, HF))),
        ":",
        two_digits(round(ratio_to_float(M, MF))),
        ":",
        two_digits(round(ratio_to_float(S, SF))),
        " UTC"
    ];
format_gps_time(Value) ->
    format_any(Value).


format_components(Components) when is_list(Components) ->
    Labels = [
        component_label(Component)
        || Component <- Components,
           Component =/= 0
    ],
    case Labels of
        [] -> format_byte_list(Components);
        _ -> join(Labels, <<", ">>)
    end;
format_components(Value) ->
    format_any(Value).


component_label(1) -> <<"Y">>;
component_label(2) -> <<"Cb">>;
component_label(3) -> <<"Cr">>;
component_label(4) -> <<"R">>;
component_label(5) -> <<"G">>;
component_label(6) -> <<"B">>;
component_label(Component) when is_integer(Component) ->
    [ "component ", integer_to_binary(Component) ];
component_label(Component) ->
    format_any(Component).


format_exposure_time(_N, 0) ->
    <<"0 sec">>;
format_exposure_time(N, D) when N > 0, D > 0, N < D ->
    RoundedDenominator = round(D / N),
    [ "1/", integer_to_binary(RoundedDenominator), " sec" ];
format_exposure_time(N, D) ->
    format_exposure_seconds(ratio_to_float(N, D)).


format_exposure_seconds(Sec) when Sec > 0, Sec < 1 ->
    [ "1/", integer_to_binary(round(1 / Sec)), " sec" ];
format_exposure_seconds(Sec) ->
    [ format_decimal(Sec, 2), " sec" ].


format_subject_area([X, Y]) when is_integer(X), is_integer(Y) ->
    [ "x ", integer_to_binary(X), ", y ", integer_to_binary(Y) ];
format_subject_area([X, Y, W, H]) when is_integer(X), is_integer(Y), is_integer(W), is_integer(H) ->
    [
        "x ", integer_to_binary(X),
        ", y ", integer_to_binary(Y),
        ", width ", integer_to_binary(W),
        ", height ", integer_to_binary(H)
    ];
format_subject_area([X, Y, D]) when is_integer(X), is_integer(Y), is_integer(D) ->
    [
        "x ", integer_to_binary(X),
        ", y ", integer_to_binary(Y),
        ", diameter ", integer_to_binary(D)
    ];
format_subject_area(Value) ->
    format_any(Value).


format_ratio(_N, 0) ->
    <<"0">>;
format_ratio(N, D) when N rem D =:= 0 ->
    integer_to_binary(N div D);
format_ratio(N, D) ->
    [ integer_to_binary(N), $/, integer_to_binary(D), " (", format_decimal(ratio_to_float(N, D), 2), ")" ].


ratio_to_float(_N, 0) ->
    0.0;
ratio_to_float(N, D) ->
    N / D.


format_exif_date(<<Y:4/binary, $:, M:2/binary, $:, D:2/binary, " ", H:2/binary, $:, I:2/binary, $:, S:2/binary>>) ->
    <<Y/binary, $-, M/binary, $-, D/binary, " ", H/binary, $:, I/binary, $:, S/binary>>;
format_exif_date(<<Y:4/binary, $:, M:2/binary, $:, D:2/binary>>) ->
    <<Y/binary, $-, M/binary, $-, D/binary>>;
format_exif_date(Value) ->
    format_any(Value).


format_ascii(Bin) when is_binary(Bin) ->
    Bin1 = trim_nul(Bin),
    case maybe_printable_binary(Bin1) of
        true -> Bin1;
        false -> format_binary_bytes(Bin)
    end;
format_ascii(Value) ->
    format_any(Value).


ascii_first(Bin) when is_binary(Bin) ->
    case trim_nul(Bin) of
        <<C, _/binary>> -> C;
        <<>> -> undefined
    end;
ascii_first([C | _]) when is_integer(C) ->
    C;
ascii_first(_) ->
    undefined.


trim_nul(Bin) ->
    case binary:match(Bin, <<0>>) of
        {Pos, _Len} -> binary:part(Bin, 0, Pos);
        nomatch -> Bin
    end.


two_digits(N) when is_integer(N), N >= 0, N < 10 ->
    [ $0, integer_to_binary(N) ];
two_digits(N) when is_integer(N) ->
    integer_to_binary(N).


format_decimal(N, 0) when is_number(N) ->
    integer_to_binary(round(N));
format_decimal(N, Decimals) when is_number(N), is_integer(Decimals), Decimals > 0 ->
    Formatted = iolist_to_binary(io_lib:format("~.*f", [Decimals, N])),
    trim_decimal(Formatted);
format_decimal(N, _Decimals) ->
    format_any(N).


trim_decimal(Bin) ->
    case binary:match(Bin, <<".">>) of
        nomatch -> Bin;
        _ -> trim_decimal_zeros(Bin)
    end.


trim_decimal_zeros(<<>>) ->
    <<"0">>;
trim_decimal_zeros(Bin) ->
    case binary:last(Bin) of
        $0 ->
            trim_decimal_zeros(binary:part(Bin, 0, byte_size(Bin) - 1));
        $. ->
            binary:part(Bin, 0, byte_size(Bin) - 1);
        _ ->
            Bin
    end.


join([], _Sep) ->
    <<>>;
join([Item], _Sep) ->
    Item;
join([Item | Rest], Sep) ->
    [ Item, Sep, join(Rest, Sep) ].


maybe_string(L) ->
    try unicode:characters_to_binary(L, utf8, utf8) of
        B when is_binary(B) ->
            case maybe_printable_binary(B) of
                true -> B;
                false -> false
            end;
        _ -> false
    catch
        _:_ -> false
    end.


format_date({{Y, M, D}, {H, I, S}}) ->
    io_lib:format(
        "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
        [Y, M, D, H, I, S]).


maybe_printable_binary(Bin) ->
    case unicode:characters_to_list(Bin, utf8) of
        L when is_list(L) ->
            lists:all(fun is_printable_char/1, L);
        _ ->
            false
    end.


is_printable_char($\t) -> true;
is_printable_char($\n) -> true;
is_printable_char($\r) -> true;
is_printable_char(C) when is_integer(C), C >= 32, C =/= 127 -> true;
is_printable_char(_) -> false.


format_binary_bytes(Bin) ->
    format_byte_list(binary_to_list(Bin)).


format_byte_list(Bytes) ->
    join([ format_byte(Byte) || Byte <- Bytes ], <<" ">>).


format_byte(Byte) when is_integer(Byte), Byte >= 0, Byte =< 255 ->
    Hex = string:uppercase(integer_to_list(Byte, 16)),
    case Hex of
        [H] -> [ "0x0", H ];
        _ -> [ "0x", Hex ]
    end;
format_byte(Byte) ->
    format_any(Byte).
