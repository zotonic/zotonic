%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2021 Marc Worrell <marc@worrell.nl>

%% @doc Extract EXIF information from the medium record to resource properties

%% Copyright 2016-2021 Marc Worrell
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

-module(mod_media_exif).

-author("Marc Worrell <marc@worrell.nl>").

-mod_title("EXIF Photos").

-mod_description("Extract EXIF information from photos when uploading").

-include_lib("zotonic_core/include/zotonic.hrl").

-export([observe_media_upload_rsc_props/3,
         medium_props/1]).

observe_media_upload_rsc_props(#media_upload_rsc_props{
                                   medium = Medium,
                                   options = Opts
                               },
                               Props,
                               _Context) ->
    Exif = maps:get(<<"exif">>, Medium, #{  }),
    PropsForced = forced_props(Props, Medium),
    PropsOverlay = overlay_props(Exif),
    Props1 = maps:merge(Props, PropsForced),
    case proplists:get_value(is_force_medium_props, Opts, true) of
        true ->
            maps:merge(Props1, PropsOverlay);
        false ->
            maps:merge(PropsOverlay, Props1)
    end.

medium_props(undefined) ->
    #{  };
medium_props(Medium) ->
    Exif = maps:get(<<"exif">>, Medium, #{  }),
    maps:merge(Exif, overlay_props(Exif)).

forced_props(Props, Medium) ->
    ImageSettings = maps:get(<<"medium_edit_settings">>, Props, #{  }),
    SubjectPoint = maps:get(<<"subject_point">>, Medium, undefined),
    #{ <<"medium_edit_settings">> => crop_center(Medium, ImageSettings, SubjectPoint) }.

overlay_props(Exif) when is_list(Exif) ->
    {ok, ExifMap} = z_props:from_list(Exif),
    overlay_props(ExifMap);
overlay_props(Exif) when is_map(Exif) ->
    DateStart = to_dt(maps:get(<<"date_time">>, Exif, maps:get(<<"date_time_digitized">>, Exif, undefined))),
    PropsOverlay =
        #{
            <<"date_start">> => DateStart,
            <<"date_end">> => DateStart,
            <<"org_pubdate">> => DateStart,
            <<"location_lat">> => gps(maps:get(<<"gps_latitude">>, Exif, undefined)),
            <<"location_lng">> => gps(maps:get(<<"gps_longitude">>, Exif, undefined))
        },
    maps:filter(fun(_K, V) ->
                   V =/= undefined
                end,
                PropsOverlay).

crop_center(Medium, undefined, Crop) ->
    crop_center(Medium, #{  }, Crop);
crop_center(#{
                <<"width">> := W,
                <<"height">> := H
            },
            Settings,
            {X, Y})
    when W > 0, H > 0 ->
    CX = z_convert:to_float(X) / z_convert:to_float(W),
    CY = z_convert:to_float(Y) / z_convert:to_float(Y),
    case CX >= 0.0 andalso CX =< 100.0 andalso CY >= 0.0 andalso CY =< 100.0 of
        true ->
            Settings#{
                        <<"crop_center_x">> => CX,
                        <<"crop_center_y">> => CY
                    };
        false ->
            Settings
    end;
crop_center(_Medium, Settings, _Crop) ->
    Settings.

% http://www.awaresystems.be/imaging/tiff/tifftags/privateifd/gps/gpslongitude.html
gps([{ratio, D, DFrac}, {ratio, M, MFrac}, {ratio, S, SFrac}] = Ratios) ->
    try
        D / DFrac + M / MFrac / 60 + S / SFrac / 3600
    catch
        _:_ ->
            ?LOG_NOTICE("mod_media_exif: illegal ratios: ~p", [Ratios]),
            undefined
    end;
gps(_) ->
    undefined.

to_dt(undefined) ->
    undefined;
to_dt(<<Y:4/binary, $:, M:2/binary, $:, D:2/binary, " ", H:2/binary, $:, I:2/binary, $:, S:2/binary>>) ->
    % <<"2015:12:04 18:05:05">>
    try
        {{z_convert:to_integer(Y), z_convert:to_integer(M), z_convert:to_integer(D)},
         {z_convert:to_integer(H), z_convert:to_integer(I), z_convert:to_integer(S)}}
    catch
        _:_ ->
            % Issue #1557: empty dates with <<"    :  :     :  :  ">>
            undefined
    end;
to_dt(DT) ->
    try
        qdate:to_date(DT)
    catch
        _:_ ->
            undefined
    end.
