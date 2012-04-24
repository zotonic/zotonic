%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Calculate a quadtile code from a lat/long location. (http://wiki.openstreetmap.org/wiki/QuadTiles)

%% Copyright 2012 Marc Worrell
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

-module(geomap_quadtile).
-author('Marc Worrell <marc@worrell.nl>').

%% Default 31 bits, as 62 bits fit in a PostgreSQL 'bigint'
-define(BITS, 31).

%% Rounding precision when decoding (ca. 10cm)
-define(PREC, 1000000).


-export([
    encode/2,
    encode/3,
    
    decode/1,
    decode/2,
    
    test/0
]).

-include_lib("zotonic.hrl").

%% @doc Calculate a quadtile number in 64 bits (32 bits per coordinate)
-spec encode(Latitude :: float(), Longitude :: float()) -> integer().
encode(Latitude, Longitude) ->
    encode(Latitude, Longitude, ?BITS).

%% @doc Calculate a quadtile number in a 2*Bits integer (Bits precision per coordinate)
-spec encode(Latitude :: float(), Longitude :: float(), Bits :: integer()) -> integer().
encode(Latitude, Longitude, Bits) ->
    interleave(to_int(Longitude, 180.0, Bits),
               to_int(Latitude, 90.0, Bits),
               Bits - 1,
               0).

%% @doc Decode the long/lat from a quadtile
-spec decode(integer()) -> {Latitude :: float(), Longitude :: float()}.
decode(Quadtile) ->
    decode(Quadtile, ?BITS).

-spec decode(Quadtile :: integer(), Bits :: integer()) -> {Latitude :: float(), Longitude :: float()}.
decode(Quadtile, Bits) ->
    {LongInt, LatInt} = deinterleave(0, 0, 1 bsl (Bits+Bits-1), Bits, Quadtile),
    {to_float(LatInt, 90.0, Bits),
     to_float(LongInt, 180.0, Bits)}.


%% @doc Interleave the longitude and latitude bit by bit, start with a longitude bit.
interleave(_, _, -1, Acc) ->
    Acc;
interleave(Long, Lat, N, Acc) ->
    Acc1 = (Acc  bsl 1) bor ((Long bsr N) band 1),
    Acc2 = (Acc1 bsl 1) bor ((Lat bsr N) band 1),
    interleave(Long, Lat, N-1, Acc2).

%% @doc Get the longitude and latitude by de-interleaving the bits
deinterleave(Long, Lat, 0, _N, _Q) ->
    {Long, Lat};
deinterleave(Long, Lat, Mask, N, Q) ->
    Long1 = Long bor ((Q band Mask) bsr N),
    Lat1 = Lat bor ((Q band (Mask bsr 1)) bsr (N-1)),
    deinterleave(Long1, Lat1, Mask bsr 2, N-1, Q).


%% @doc Map a longitude/latitude to an integer in the range 0..(2**Bits-1)
to_int(Angle, MaxAngle, Bits) ->
    round((fmod(Angle, MaxAngle)+MaxAngle)/(2.0*MaxAngle) * ((1 bsl Bits)-1)).

%% @doc Map a N bits integer to a float longitude/latitude
to_float(N, MaxAngle, Bits) ->
    round(((N / ((1 bsl Bits) -1))*2.0*MaxAngle - MaxAngle) * ?PREC) / ?PREC.


%% @doc Ensure that the angle is between [-Max, Max>
fmod(Angle, Max) when Angle < -Max ->
    fmod(Angle+2.0*Max, Max);
fmod(Angle, Max) when Angle >= Max ->
    fmod(Angle-2.0*Max, Max);
fmod(Angle, _Max) ->
    Angle.


test() ->
    0 = geomap_quadtile:encode(-90, -180),
    0 = geomap_quadtile:encode(90, 180),
    {-90.0,-180.0} = geomap_quadtile:decode(0),
    {1.0, 2.0} = geomap_quadtile:decode(geomap_quadtile:encode(1,2)),
    % Below is close to (1 bsl (?BITS*2))-1 = 4611686018427387903
    4611686018427387900 = geomap_quadtile:encode(89.9999999, 179.9999999),
    {90.0, 180.0} = geomap_quadtile:decode(4611686018427387900),
    ok.


