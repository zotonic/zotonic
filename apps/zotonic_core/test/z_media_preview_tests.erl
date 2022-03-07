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
    ?assertEqual(
        "-strip "
        "-coalesce    -colorspace \"sRGB\" -gravity NorthWest "
        "-crop 80x80+21+0 -extent 80x80 +repage -set units PixelsPerInch"
        " -density 72  ", CmdArgs).

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
