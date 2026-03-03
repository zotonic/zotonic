%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @hidden

-module(z_media_preview_tests).

-include_lib("eunit/include/eunit.hrl").


cmd_args_jpeg_test() ->
    MediaProps = #{
        <<"width">> => 100,
        <<"height">> => 66,
        <<"mime">> => <<"image/jpeg">>,
        <<"orientation">> => 1
    },
    Filters = [
        {crop, center},
        {width, 80},
        {height, 80}
    ],
    {ok, {_W,_H,Args}} = z_media_preview:cmd_args(MediaProps, Filters, <<"image/jpeg">>),
    CmdArgs = lists:flatten(lists:join(32, Args)),
    ?assertEqual(
        "-strip "
        "-background \"white\" -layers \"flatten\"    -colorspace \"sRGB\" "
        "-gravity NorthWest -crop 80x80+21+0 -extent 80x80 +repage -set units PixelsPerInch "
        "-density 72   -unsharp 0.3x0.7  -quality 97 -interlace \"plane\"", CmdArgs).


cmd_args_gif_test() ->
    MediaProps = #{
        <<"width">> => 100,
        <<"height">> => 66,
        <<"mime">> => <<"image/gif">>,
        <<"orientation">> => 1
    },
    Filters = [
        {crop, center},
        {width, 80},
        {height, 80}
    ],
    {ok, {_W,_H,Args}} = z_media_preview:cmd_args(MediaProps, Filters, <<"image/gif">>),
    CmdArgs = lists:flatten(lists:join(32, Args)),
    Deconstruct = case z_media_preview:is_legacy_imagemagick() of
        true -> "-deconstruct";
        false -> "-layers \"CompareAny\""
    end,
    ?assertEqual(
        "-strip "
        "-coalesce    -colorspace \"sRGB\" -gravity NorthWest "
        "-crop 80x80+21+0 -extent 80x80 +repage -set units PixelsPerInch"
        " -density 72   " ++ Deconstruct, CmdArgs).

media_data_url_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    Data = z_media_tag:scomp_data_url(<<"lib/images/trans.gif">>, [], Context),
    ?assertEqual(Data, <<"data:image/gif;base64,R0lGODlhAQABAJAAAAAAAAAAACH5BAEUAAAALAAAAAABAAEAAAICRAEAOw==">>).


calc_size_test() ->
    FourThirds = #{ image_width => 4000, image_height => 3000 },

    ?assertEqual({200, 150, none}, z_media_preview:calc_size(FourThirds#{ req_width => 200, req_height => 200, crop => none })),
    ?assertEqual({200, 150, none}, z_media_preview:calc_size(FourThirds#{ req_width => 200, req_height => 200, crop => none, orientation => 1 })),
    ?assertEqual({200, 150, none}, z_media_preview:calc_size(FourThirds#{ req_width => 200, req_height => 200, crop => none, orientation => 2 })),
    ?assertEqual({200, 150, none}, z_media_preview:calc_size(FourThirds#{ req_width => 200, req_height => 200, crop => none, orientation => 3 })),
    ?assertEqual({200, 150, none}, z_media_preview:calc_size(FourThirds#{ req_width => 200, req_height => 200, crop => none, orientation => 4 })),
    ?assertEqual({150, 200, none}, z_media_preview:calc_size(FourThirds#{ req_width => 200, req_height => 200, crop => none, orientation => 5 })),
    ?assertEqual({150, 200, none}, z_media_preview:calc_size(FourThirds#{ req_width => 200, req_height => 200, crop => none, orientation => 6 })),
    ?assertEqual({150, 200, none}, z_media_preview:calc_size(FourThirds#{ req_width => 200, req_height => 200, crop => none, orientation => 7 })),
    ?assertEqual({150, 200, none}, z_media_preview:calc_size(FourThirds#{ req_width => 200, req_height => 200, crop => none, orientation => 8 })),

    ok.

%% Tests for imagemagick_detect/1, the injectable detection helper.
%% Each test supplies a finder function that simulates a specific installation
%% scenario, allowing deterministic verification of the v6/v7 logic.

imagemagick_detect_v7_test() ->
    %% Simulate ImageMagick v7: only 'magick' is present.
    FindExe = fun("magick") -> "/usr/bin/magick"; (_) -> false end,
    #{ cmd := Cmd, legacy := Legacy } = z_media_preview:imagemagick_detect(FindExe),
    ?assertEqual(false, Legacy),
    ?assertNotEqual(false, Cmd).

imagemagick_detect_v6_test() ->
    %% Simulate legacy ImageMagick v6: 'magick' absent, 'convert' present.
    FindExe = fun("magick") -> false; (_) -> "/usr/bin/convert" end,
    #{ cmd := Cmd, legacy := Legacy } = z_media_preview:imagemagick_detect(FindExe),
    ?assertEqual(true, Legacy),
    ?assertNotEqual(false, Cmd).

imagemagick_detect_none_test() ->
    %% Simulate no ImageMagick installation at all.
    FindExe = fun(_) -> false end,
    #{ cmd := Cmd, legacy := Legacy } = z_media_preview:imagemagick_detect(FindExe),
    ?assertEqual(true, Legacy),
    ?assertEqual(false, Cmd).
