%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Show a location's map using static images from OpenStreetMap

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

-module(scomp_geomap_geomap_static).
-author('Marc Worrell <marc@worrell.nl>').
-behaviour(gen_scomp).

-export([
    vary/2, render/3
]).

-define(TILE_WIDTH, 256).
-define(TILE_HEIGHT, 256).


-include("zotonic.hrl").

vary(_Params, _Context) -> nocache.

render(Params, _Vars, Context) ->
    case get_latlong(Params, Context) of
        {Latitude, Longitude} when is_float(Latitude), is_float(Longitude) ->
            Zoom = z_convert:to_integer(proplists:get_value(zoom, Params, geomap_tiles:zoom())),
            N = z_convert:to_integer(proplists:get_value(n, Params, 2)),
            Cols = z_convert:to_integer(proplists:get_value(cols, Params, N)),
            Rows = z_convert:to_integer(proplists:get_value(rows, Params, N)),
            case geomap_tiles:map_tiles(Latitude, Longitude, Cols, Rows, Zoom) of
                {ok, Tiles, {MarkerX,MarkerY}} ->
                    Vars = [
                        {n, N},
                        {rows, Rows},
                        {cols, Cols},
                        {location_lat, Latitude},
                        {location_lng, Longitude},
                        {tiles, Tiles},
                        {marker, {MarkerX,MarkerY}},
                        {marker_px, {round(MarkerX*?TILE_WIDTH), round(MarkerY*?TILE_HEIGHT)}}
                        | Params
                    ],
                    {Html, _Context} = z_template:render_to_iolist("_geomap_static.tpl", Vars, Context),
                    {ok, Html};
                _ ->
                    {ok, <<>>}
            end;
        _ ->
            {ok, <<>>}
    end.



get_latlong(Params, Context) ->
    case proplists:get_value(latitude, Params) of
        undefined ->
            case proplists:get_value(id, Params) of
                undefined ->
                    {undefined, undefined};
                Id -> 
                    case m_rsc:rid(Id, Context) of
                        undefined ->
                            {undefined, undefined};
                        RId ->
                            {m_rsc:p(RId, computed_location_lat, Context),
                             m_rsc:p(RId, computed_location_lng, Context)}
                    end
            end;
        Lat ->
            {catch z_convert:to_float(Lat),
             catch z_convert:to_float(proplists:get_value(longitude, Params))}
    end.



