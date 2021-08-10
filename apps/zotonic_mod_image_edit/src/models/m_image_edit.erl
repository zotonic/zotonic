%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Model for fetching the image edit settings of a page.

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

-module(m_image_edit).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,

    get_settings/2,
    save_settings/3,
    sanitize/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"settings">>, Id | Rest ], _Msg, Context) ->
    {ok, Settings} = get_settings(Id, Context),
    {ok, {maps:merge(empty_args(), Settings), Rest}}.

-spec get_settings( m_rsc:resource(), z:context() ) -> {ok, map()}.
get_settings(Id, Context) ->
    case m_rsc:p(Id, <<"medium_edit_settings">>, Context) of
        undefined ->
            maybe_convert_crop_center(Id, Context);
        Settings when is_map(Settings) ->
            {ok, drop_neutral(Settings)};
        _ ->
            {ok, #{}}
    end.

-spec save_settings( m_rsc:resource(), undefined | proplists:proplist() | map(), z:context() ) -> {ok, m_rsc:resource_id()} | {error, term()}.
save_settings(Id, undefined, Context) ->
    m_rsc:update(Id, #{ <<"medium_edit_settings">> => undefined }, Context);
save_settings(Id, Settings, Context) when is_list(Settings) ->
    Settings1 = lists:foldl(
        fun({K, V}, Acc) ->
            Acc#{ z_convert:to_binary(K) => V }
        end,
        #{},
        Settings),
    save_settings(Id, Settings1, Context);
save_settings(Id, Settings, Context) ->
    Settings1 = sanitize(Settings),
    m_rsc:update(Id, #{ <<"medium_edit_settings">> => Settings1 }, Context).


-spec sanitize( map() ) -> map().
sanitize(Settings) ->
    drop_neutral(
        maps:fold(
            fun(K, V, Acc) ->
                K1 = z_convert:to_binary(K),
                case maps:is_key(K1, empty_args()) of
                    true ->
                        case normalize(K1, V) of
                            undefined -> Acc;
                            V1 -> Acc#{ K1 => V1 }
                        end;
                    false ->
                        Acc
                end
            end,
            #{},
            Settings)).

% Check for older 'crop_center' property
maybe_convert_crop_center(Id, Context) ->
    case m_rsc:p(Id, <<"crop_center">>, Context) of
        undefined ->
            {ok, #{}};
        <<>> ->
            {ok, #{}};
        <<"+", CropCenter/binary>> ->
            try
                % The 'crop_center' property is defined in pixels.
                % Our crop_center_x/y arguments are defined in fractions.
                [X, Y] = binary:split(CropCenter, <<"+">>),
                X1 = z_convert:to_float(X),
                Y1 = z_convert:to_float(Y),
                Medium = m_media:get(Id, Context),
                #{
                    <<"width">> := W,
                    <<"height">> := H
                } = Medium,
                {ok, #{
                    <<"crop_center_x">> => X1 / W,
                    <<"crop_center_y">> => Y1 / H
                }}
            catch
                _:_ -> {ok, #{}}
            end;
        _ ->
            {ok, #{}}
    end.


normalize(<<"is_lossless">>, V) ->
    z_convert:to_bool(V);
normalize(<<"crop_center_", _/binary>>, V) ->
    case z_convert:to_float(V) of
        undefined -> undefined;
        V1 when V1 < 0.0 -> undefined;
        V1 when V1 > 100.0 -> 100.0;
        V1 -> round(V1*100000) / 100000
    end;
normalize(<<"crop_", _/binary>>, V) ->
    case z_convert:to_float(V) of
        undefined -> undefined;
        V1 when V1 < 0.0 -> 0.0;
        V1 when V1 > 100.0 -> 100.0;
        V1 -> round(V1*10) / 10
    end;
normalize(<<"rotation">>, V) ->
    case z_convert:to_integer(V) of
        -90 -> -90;
        -180 -> -180;
        -270 -> -270;
        90 -> -270;
        180 -> -180;
        270 -> -90;
        _ -> 0
    end;
normalize(K, V)
    when K =:= <<"contrast">>;
         K =:= <<"brightness">> ->
    case z_convert:to_integer(V) of
        undefined -> undefined;
        V1 when V1 > 100 -> 100;
        V1 when V1 < -100 -> -100;
        V1 -> V1
    end;
normalize(<<"roll">>, V) ->
    case z_convert:to_integer(V) of
        undefined -> undefined;
        V1 when V1 > 45 -> 45;
        V1 when V1 < -45 -> -45;
        V1 -> V1
    end;
normalize(K, V)
    when K =:= <<"pan">>;
         K =:= <<"tilt">> ->
    case z_convert:to_integer(V) of
        undefined -> undefined;
        V1 when V1 > 180 -> 180;
        V1 when V1 < -180 -> -180;
        V1 -> V1
    end;
normalize(K, _) ->
    lager:info("Dropping unknown image edit argument ~p", [ K ]),
    undefined.


drop_neutral(Settings) ->
    maps:filter(
        fun(K, V) ->
            case maps:get(K, empty_args(), undefined) of
                undefined -> false;
                V -> false;
                _ -> true
            end
        end,
        Settings).


empty_args() ->
    #{
        <<"is_lossless">> => false,

        <<"crop_center_x">> => -1,
        <<"crop_center_y">> => -1,

        <<"rotation">> => 0,

        <<"crop_top">> => 0.0,
        <<"crop_bottom">> => 0.0,
        <<"crop_left">> => 0.0,
        <<"crop_right">> => 0.0,

        <<"contrast">> => 0,
        <<"brightness">> => 0,

        <<"roll">> => 0,
        <<"pan">> => 0,
        <<"tilt">> => 0
    }.
