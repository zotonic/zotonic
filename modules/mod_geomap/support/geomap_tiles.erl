-module(geomap_tiles).

-export([
    map_tiles/4,
    map_tiles/5,
    
    url_tile/2,
    url_tile/3,
    
    xy_tile/2,
    xy_tile/3,
    
    bounding_box/3,
    
    zoom/0
]).

-include("zotonic.hrl").

%% From http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#lon.2Flat_to_tile_numbers

%% @doc Default zoom factor for displaying the tiles
-define(ZOOM, 14).

%% @doc Return the default zoom factor
-spec zoom() -> integer().
zoom() ->
    ?ZOOM.

%% @doc Return the {x,y,z} tuples for 3x3 map and the offset of the point from the upper-left tile.
-spec map_tiles(Latitude::float(), Longitude::float(), Cols::integer(), Rows::integer()) -> {ok,list(),{integer(),integer()}}.
map_tiles(Latitude, Longitude, Cols, Rows) ->
    map_tiles(Latitude, Longitude, Cols, Rows, ?ZOOM).
    
-spec map_tiles(Latitude::float(), Longitude::float(), Cols::integer(), Rows:: integer(), Zoom::integer()) -> {ok,list(),{float(),float()}}.
map_tiles(Latitude, Longitude, Cols, Rows, Zoom) ->
    {X, Y} = xy_tile(Latitude, Longitude, Zoom),
    BoundingBox = bounding_box(X,Y,Zoom),
    {CX,CY} = correct_xy(Latitude, Longitude, X, Y, Cols, Rows, BoundingBox),
    TileRows = to_rows(CX, CY, Cols, Rows, Zoom),
    Tiles = [
        [ {Xt,Yt,Zoom} || {Xt,Yt} <- Row ]
        || Row <- TileRows
    ],
    {ok, Tiles, offset(X,Y,TileRows,Latitude,Longitude,BoundingBox)}.

    % Correct x/y depending if the lat/long is the closest edge of the tile
    correct_xy(Latitude, Longitude, X, Y, Cols, Rows, {North,South,East,West}) ->
        HalfX = abs(East-West) / 2.0,
        HalfY = abs(North-South) / 2.0,
        OffsetX = abs(West-Longitude),
        OffsetY = abs(North-Latitude),
        CenteredX = case Cols rem 2 of
                        0 ->
                            case OffsetX < HalfX of
                                true -> X+1;
                                false -> X
                            end;
                        1 -> X
                    end,
        CenteredY = case Rows rem 2 of
                        0 -> 
                            case OffsetY < HalfY of
                                true -> Y;
                                false -> Y+1
                            end;
                        1 -> Y
                    end,
        {CenteredX,CenteredY}.

    % Calculate the offset in tiles for Lat/Long from the upperleft corner of the map
    offset(X,Y,TileRows,Latitude,Longitude,{North,South,East,West}) ->
        XTile = offset(fun({Xx,_}) -> Xx =:= X end, hd(TileRows), 0),
        YTile = offset(fun([{_,Yy}|_]) -> Yy =:= Y end, TileRows, 0),
        LongOffset = abs(West-Longitude),
        LatOffset = abs(North-Latitude),
        LongPerTile = abs(East-West),
        LatPerTile = abs(North-South),
        {float(XTile) + (1-LongOffset/LongPerTile), 
         float(YTile) + LatOffset/LatPerTile}.
    
    offset(F, [H|T], N) ->
        case F(H) of
            true -> N;
            false -> offset(F,T,N+1)
        end.

%% @doc Generate the tile URL for a coordinate
-spec url_tile(Latitude::float(), Longitude::float()) -> URL::binary().
url_tile(Latitude, Longitude) ->
    url_tile(Latitude, Longitude, ?ZOOM).

%% @doc Generate the tile URL for a coordinate and zoom factor
-spec url_tile(Latitude::float(), Longitude::float(), Zoom::integer()) -> URL::binary().
url_tile(Latitude, Longitude, Zoom) ->
    {Xtile, Ytile} = xy_tile(Latitude, Longitude, Zoom),
    xyz_url(Xtile, Ytile, Zoom).


%% @doc Return the bounding box in coordinates {North, South, East, West} for a tile.
bounding_box(X, Y, Zoom) ->
    {tile2lat(Y, Zoom), tile2lat(Y+1,Zoom),
     tile2long(X, Zoom), tile2long(X+1, Zoom)}.

tile2lat(Y, Zoom) ->
    N = math:pi() - (2.0 * math:pi() * Y) / math:pow(2.0, Zoom),
    (180.0 / math:pi() * math:atan(0.5 * (math:exp(N) - math:exp(-N)))).

tile2long(X, Zoom) ->
    (X / math:pow(2.0, Zoom) * 360.0 - 180.0).


%% @doc Calculate the points around the XY point.
to_rows(X,Y,Cols,Rows,Zoom) ->
    Max = 1 bsl Zoom,
    [
        to_row(X,Y1,Cols,Max)
        || Y1 <- row_range(Y,Rows,Max)
    ].

    row_range(Y,1,_Max) ->
        [Y];
    row_range(Y,N,Max) ->
        From = Y - N div 2,
        To = From + N-1,
        if
            From < 0 ->
                lists:seq(0,N-1);
            To >= Max ->
                lists:seq(Max-N, Max-1);
            true ->
                lists:seq(From, To)
        end.

    to_row(X,Y,Cols,Max) ->
        From = X - Cols div 2,
        [ norm(X1,Y,Max) || X1 <- lists:seq(From, From+Cols-1) ].

    norm(X,Y,Max) when X < 0 ->
        {X+Max, Y};
    norm(X,Y,Max) ->
        {X rem Max, Y}.


%% @doc Conncatenate the X, Y and Zoom factor into an OpenStreetMap tile URL
xyz_url(X,Y,Zoom) ->
    iolist_to_binary([
            "http://tile.openstreetmap.org",
            $/, integer_to_list(Zoom),
            $/, integer_to_list(X),
            $/, integer_to_list(Y),
            ".png"
        ]).

%% @doc Calculate the xy coordinate of the tile for the lat/long
xy_tile(Latitude, Longitude) ->
    xy_tile(Latitude, Longitude, ?ZOOM).
xy_tile(Latitude, Longitude, Zoom) ->
    {long2tile(Longitude, Zoom), lat2tile(Latitude, Zoom)}.

long2tile(Longitude, Zoom) ->
    floor((Longitude+180.0)/360.0 * math:pow(2,Zoom)).

lat2tile(Latitude, Zoom) ->
    floor((1-math:log(math:tan(Latitude*math:pi()/180.0) + 1/math:cos(Latitude*math:pi()/180)) / math:pi()) /2.0 * math:pow(2,Zoom)).

%% From http://schemecookbook.org/Erlang/NumberRounding
floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.
