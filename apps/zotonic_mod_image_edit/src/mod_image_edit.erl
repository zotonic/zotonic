%% @copyright 2021 Marc Worrell
%% @author Marc Worrell <marc@worrell.nl>
%% @doc Add functionality to the admin to edit images.

%% Copyright 2021 Marc Worrell
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

-module(mod_image_edit).

-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Image Edit").

-mod_description("Edit uploaded images.").

-mod_prio(500).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([event/2,
         observe_media_upload_rsc_props/3,
         observe_rsc_update/3,
         observe_media_preview_options/3]).

event(#submit{ message = {edit_form, Args} }, Context) ->
    {id, ArgId} = proplists:lookup(id, Args),
    OnSuccess = proplists:get_all_values(on_success, Args),
    Id = m_rsc:rid(ArgId, Context),
    case z_acl:rsc_editable(Id, Context) of
        true ->
            Settings = z_context:get_q_all_noz(Context),
            case m_image_edit:save_settings(Id, Settings, Context) of
                {ok, _} ->
                    Context1 = z_render:growl(?__("Saved the image settings.", Context), Context),
                    z_render:wire(OnSuccess, Context1);
                {error, _} ->
                    z_render:growl_error(?__("Could not save the image settings.", Context), Context)
            end;
        false ->
            z_render:growl_error(?__("You don't have permission to edit this page.", Context), Context)
    end.

observe_media_upload_rsc_props(#media_upload_rsc_props{  }, Props, _Context) ->
    case maps:get(<<"medium_edit_settings">>, Props, undefined) of
        M when is_map(M) ->
            Props;
        _ ->
            Props#{ <<"medium_edit_settings">> => #{  } }
    end.

observe_rsc_update(#rsc_update{  }, {ok, #{ <<"medium_edit_settings">> := Settings } = Props}, _Context) ->
    Sanitized = m_image_edit:sanitize(Settings),
    {ok, Props#{ <<"medium_edit_settings">> => Sanitized }};
observe_rsc_update(_, Acc, _Context) ->
    Acc.

%% @doc Expand the image modifications that influence the generated preview image.
%% The order of the operations is important. As they are added in front of the
%% options list the last added option will be performed first.
%% The order of the operations is:
%%
%% <ol>
%% <li>Lossless (just an option for later image compression)</li>
%% <li>Angle</li>
%% <li>Tilt</li>
%% <li>Pan</li>
%% <li>Rotate  (90deg increments)</li>
%% <li>Crop</li>
%% <li>Contrast</li>
%% <li>Brightness</li>
%% </ol>
%%
observe_media_preview_options(#media_preview_options{ id = undefined }, Options, _Context) ->
    Options;
observe_media_preview_options(#media_preview_options{
                                  id = Id,
                                  width = W,
                                  height = H
                              },
                              Options,
                              Context) ->
    {ok, Map} = m_image_edit:get_settings(Id, Context),
    expand_edit_settings(Map, W, H, Options, Context).

expand_edit_settings(Settings, W, H, Options, Context) ->
    % Contrast & brightness are last
    OptionsB =
        case maps:get(<<"brightness">>, Settings, 0) of
            0 ->
                Options;
            Brightness ->
                [{brightness, Brightness} | Options]
        end,
    OptionsCB =
        case maps:get(<<"contrast">>, Settings, 0) of
            0 ->
                OptionsB;
            Contrast ->
                [{contrast, Contrast} | OptionsB]
        end,

    % Crop before contrast & brightness
    CL = maps:get(<<"crop_left">>, Settings, 0.0),
    CR = maps:get(<<"crop_right">>, Settings, 0.0),
    CT = maps:get(<<"crop_top">>, Settings, 0.0),
    CB = maps:get(<<"crop_bottom">>, Settings, 0.0),

    OptionsCrop =
        case nz(CL) orelse nz(CR) orelse nz(CT) orelse nz(CB) of
            true ->
                [{cropp, [CL, CR, CT, CB]} | OptionsCB];
            false ->
                OptionsCB
        end,

    % Rotate before crop
    Options6 =
        case maps:get(<<"rotate">>, Settings, 0) of
            0 ->
                OptionsCrop;
            R ->
                [{rotate, R} | OptionsCrop]
        end,

    % Tilt, angle and pan before rotate
    Roll = maps:get(<<"roll">>, Settings, 0),
    Tilt = maps:get(<<"tilt">>, Settings, 0),
    Pan = maps:get(<<"pan">>, Settings, 0),
    Options7 =
        case {Roll, Tilt, Pan} of
            {0, 0, 0} ->
                Options6;
            _ ->
                [{rotate3d, [Roll, Tilt, Pan]} | Options6]
        end,

    % Lossless as first
    OptionsLL =
        case maps:get(<<"is_lossless">>, Settings, false) of
            true ->
                [{lossless, true} | Options7];
            false ->
                Options7
        end,

    % Crop center - defined in fractions - recalculated to pixels on the original image.
    % The image preview routines will map the crop coordinate using cropp and rotation.
    CropX = maps:get(<<"crop_center_x">>, Settings, -1),
    CropY = maps:get(<<"crop_center_y">>, Settings, -1),
    OptionsCropCenter =
        case CropX >= 0.0 andalso CropY >= 0.0 of
            true ->
                CropXPix = trunc(CropX * W),
                CropYPix = trunc(CropY * H),
                opt_crop_center([CropXPix, CropYPix], OptionsLL, Context);
            false ->
                OptionsLL
        end,
    OptionsCropCenter.

% Test if a floating point value is non-zero
nz(V) ->
    V >= 0.1 orelse V =< -0.1.

opt_crop_center(CropXY, Options, Context) ->
    Crop = proplists:get_value(crop, Options),
    case is_crop_center(Crop) of
        true ->
            add_crop_center(CropXY, Options);
        false when Crop =:= undefined ->
            opt_crop_center_mediaclass(proplists:get_value(mediaclass, Options), CropXY, Options, Context);
        false ->
            Options
    end.

opt_crop_center_mediaclass(undefined, _CropXY, Options, _Context) ->
    Options;
opt_crop_center_mediaclass(Mediaclass, CropXY, Options, Context) ->
    {ok, Props, _Hash} = z_mediaclass:get(Mediaclass, Context),
    case is_crop_center(proplists:get_value(crop, Props)) of
        true ->
            add_crop_center(CropXY, Options);
        false ->
            Options
    end.

add_crop_center(CropXY, Options) ->
    z_utils:prop_replace(crop, CropXY, Options).

is_crop_center(true) ->
    true;
is_crop_center(center) ->
    true;
is_crop_center(<<"center">>) ->
    true;
is_crop_center("center") ->
    true;
is_crop_center(_) ->
    false.
