%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Calculate the ImageMagick distortion parameters for a 3D rotation.

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

-module(z_media_rotate3d).

-export([
    rotate3d/5
]).

-include("../../include/zotonic.hrl").

%% @doc Calculate the transformation matrix for the perspective. Translate the corner
%% points of the image to the arguments for the ImageMagick distort function. This code
%% has been derived from http://www.fmwconcepts.com/imagemagick/3Drotate/index.php
%%
%% Here we don't scale the image, the image is rotated from the center of the image, and
%% we use a fixed viewing point that is compatible with the viewing point used in the
%% mod_image_edit css3 transformations.
-spec rotate3d( Width :: non_neg_integer(), Height :: non_neg_integer(),
                Roll :: number(), Tilt :: number(), Pan :: number() ) -> {ok, string()} | {error, term()}.
rotate3d(Width, Height, _Roll, _Tilt, _Pan) when Width =< 1; Height =< 1 ->
    {error, empty_image};
rotate3d(Width, Height, Roll, Tilt, Pan) when is_number(Roll), is_number(Tilt), is_number(Pan) ->
    % Calculate the projection matrices for the roll, tilt and pan
    R_Roll = roll(Roll),
    R_Tilt = tilt(Tilt),
    R_Pan = pan(Pan),

    % Start with the identity matrix
    R0 = [
        [ 1.0, 0.0, 0.0 ],
        [ 0.0, 1.0, 0.0 ],
        [ 0.0, 0.0, 1.0 ]
    ],

    % Apply the rotations to the identity matrix
    R1 = multiply_matrix(R_Roll, R0),
    R2 = multiply_matrix(R_Tilt, R1),
    R = multiply_matrix(R_Pan, R2),

    % Apply correction for the position/distance of the viewer.
    % Dfov = 56.3099324782,
    % DfovRad = deg2rad(Dfov / 2),
    % Sfov = math:sin(DfovRad),
    % Cfov = math:cos(DfovRad),
    % Tfov = Sfov / Cfov,
    Tfov = 0.5351837583,
    Diag = math:sqrt( (Width * Width) + (Height * Height) ),
    Focal = Diag / (2 * Tfov),

    DI = (Width - 1.0) / 2.0,
    DJ = (Height - 1.0) / 2.0,

    % Offset / conversion matrix
    B = [
        [ 1.0,  0.0, -DI ],
        [ 0.0, -1.0,  DJ ],
        [ 0.0,  0.0,  1.0 ]
    ],
    % Output scaling matrix
    Aim = [
        [ 1.0,  0.0, -DI / Focal ],
        [ 0.0, -1.0, -DJ / Focal ],
        [ 0.0,  0.0, -1 / Focal ]
    ],

    [
        [ R_00, R_01, _R_02 ],
        [ R_10, R_11, _R_12 ],
        [ R_20, R_21, _R_22 ]
    ] = R,
    T = [
        [ R_00, R_01, 0 ],
        [ R_10, R_11, 0 ],
        [ R_20, R_21, - Focal ]
    ],

    P0 = multiply_matrix(T, B),
    P = multiply_matrix(Aim, P0),

    % Project the corners of the image to the new coordinates.
    {UpLeft_X, UpLeft_Y} = forward_project(0, 0, P),
    {UpRight_X, UpRight_Y} = forward_project(Width-1, 0, P),
    {BottomRight_X, BottomRight_Y} = forward_project(Width-1, Height-1, P),
    {BottomLeft_X, BottomLeft_Y} = forward_project(0, Height-1, P),

    % Build the ImageMagick command argument
    D = "-distort Perspective \"" ++ lists:flatten([
        io_lib:format("~p,~p ~p,~p", [
            0,0,  UpLeft_X,UpLeft_Y
            ]),
        " ",
        io_lib:format("~p,~p ~p,~p", [
            Width-1,0,  UpRight_X,UpRight_Y
            ]),
        " ",
        io_lib:format("~p,~p ~p,~p", [
            Width-1,Height-1,  BottomRight_X,BottomRight_Y
            ]),
        " ",
        io_lib:format("~p,~p ~p,~p", [
            0,Height-1,  BottomLeft_X,BottomLeft_Y
            ]),
        "\""
        ]),
    {ok, D}.


% Transformation matrices for pan, tilt and angle.
pan(Pan) when Pan >= -180, Pan =< 180->
    PanRad = deg2rad(Pan),
    SinPan = math:sin(PanRad),
    CosPan = math:cos(PanRad),
    [
        [ CosPan, 0, SinPan ],
        [ 0, 1, 0 ],
        [ -SinPan, 0, CosPan ]
    ].

tilt(Tilt) when Tilt >= -180, Tilt =< 180->
    TiltRad = deg2rad(Tilt),
    SinTilt = math:sin(TiltRad),
    CosTilt = math:cos(TiltRad),
    [
        [ 1, 0, 0 ],
        [ 0, CosTilt, SinTilt ],
        [ 0, -SinTilt, CosTilt ]
    ].

roll(Roll) when Roll >= -180, Roll =< 180->
    RollRad = deg2rad(Roll),
    SinRoll = math:sin(RollRad),
    CosRoll = math:cos(RollRad),
    [
        [ CosRoll, SinRoll, 0 ],
        [ -SinRoll, CosRoll, 0 ],
        [ 0, 0, 1 ]
    ].

deg2rad(Angle) ->
    math:pi() * Angle / 180.0.


% Project a point from the input image to the output image.
forward_project(X, Y, M) ->
    [
        [ P_00, P_01, P_02 ],
        [ P_10, P_11, P_12 ],
        [ P_20, P_21, P_22 ]
    ] = M,
    NumU = (P_00 * X) + (P_01 * Y) + P_02,
    NumV = (P_10 * X) + (P_11 * Y) + P_12,
    Den = (P_20 * X) + (P_21 * Y) + P_22,
    {trunc(NumU / Den), trunc(NumV / Den)}.


%%%%%%%%%%  Matrix functions %%%%%%%%%%

% From: http://zxq9.com/archives/1387

%%% @doc
%%% A naive matrix generation, rotation and multiplication module.
%%% It doesn't concern itself with much checking, so input dimensions must be known
%%% prior to calling any of these functions lest you receive some weird results back,
%%% as most of these functions do not crash on input that go against the rules of
%%% matrix multiplication.
%%%
%%% All functions crash on obviously bad values.
%%% @end

-type matrix() :: [[number()]].

%% @doc
%% Takes a matrix of {X, Y} size and rotates it left, returning a matrix of {Y, X} size.
-spec rotate_matrix(matrix()) -> matrix().
rotate_matrix(Matrix) ->
    rotate_matrix(Matrix, [], [], []).


%% @private
%% Iterates doubly over a matrix, packing the diminished remainder into Rem and
%% packing the current row into Current. This is naive, in that it assumes an
%% even matrix of dimentions {X, Y}, and will return one of dimentions {Y, X}
%% based on the length of the first row, regardless whether the input was actually
%% even.
-spec rotate_matrix(Matrix, Rem, Current, Acc) -> Rotated
    when Matrix  :: matrix(),
         Rem     :: [[number()]],
         Current :: [number()],
         Acc     :: matrix(),
         Rotated :: matrix().
rotate_matrix([[] | _], [], [], Acc) ->
    Acc;
rotate_matrix([], Rem, Current, Acc) ->
    NewRem = lists:reverse(Rem),
    NewCurrent = lists:reverse(Current),
    rotate_matrix(NewRem, [], [], [NewCurrent | Acc]);
rotate_matrix([[V | Vs] | Rows], Rem, Current, Acc) ->
    rotate_matrix(Rows, [Vs | Rem], [V | Current], Acc).


%% @doc
%% Multiply two matrices together according to the matrix multiplication rules.
%% This function does not check that the inputs are actually proper (regular)
%% matrices, but does check that the input row/column lengths are compatible.
-spec multiply_matrix(A, B) -> Product
    when A       :: matrix(),
         B       :: matrix(),
         Product :: matrix().
multiply_matrix(A = [R | _], B) when length(R) == length(B) ->
    multiply_matrix(A, rotate_matrix(B), []).


%% @private
%% Iterate a row multiplication operation of each row of A over matrix B until
%% A is exhausted.
-spec multiply_matrix(A, B, Acc) -> Product
    when A       :: matrix(),
         B       :: matrix(),
         Acc     :: matrix(),
         Product :: matrix().
multiply_matrix([A | As], B, Acc) ->
    Prod = multiply_row(A, B, []),
    multiply_matrix(As, B, [Prod | Acc]);
multiply_matrix([], _, Acc) ->
    lists:reverse(Acc).


%% @private
%% Multiply each row of matrix B by the input Row, returning the list of resulting sums.
-spec multiply_row(Row, B, Acc) -> Product
    when Row     :: [number()],
         B       :: matrix(),
         Acc     :: [number()],
         Product :: [number()].
multiply_row(Row, [B | Bs], Acc) ->
    ZipProd = lists:zipwith(fun(X, Y) -> X * Y end, Row, B),
    Sum = lists:sum(ZipProd),
    multiply_row(Row, Bs, [Sum | Acc]);
multiply_row(_, [], Acc) ->
    Acc.

