%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @hidden

-module(z_media_preview_tests).

-include_lib("eunit/include/eunit.hrl").


cmd_args_jpeg_test() ->
    Props = [{width,100}, {height,66}, {mime,"image/jpeg"}, {orientation,1}],
    Filters = [{crop,center}, {width,80}, {height,80}],
    {_W,_H,Args} = z_media_preview:cmd_args(Props, Filters, <<"image/jpeg">>),
    CmdArgs = lists:flatten(lists:join(32, Args)),
    ?assertEqual("-background \"white\" -layers \"flatten\"    -gravity NorthWest -crop 80x80+21+0 -extent 80x80 +repage -colorspace \"sRGB\" -set units PixelsPerInch -density 72   -unsharp 0.3x0.7  -quality 97", CmdArgs).


cmd_args_gif_test() ->
    Props = [{width,100}, {height,66}, {mime,"image/gif"}, {orientation,1}],
    Filters = [{crop,center}, {width,80}, {height,80}],
    {_W,_H,Args} = z_media_preview:cmd_args(Props, Filters, <<"image/gif">>),
    CmdArgs = lists:flatten(lists:join(32, Args)),
    ?assertEqual("-coalesce    -gravity NorthWest -crop 80x80+21+0 -extent 80x80 +repage -colorspace \"sRGB\" -set units PixelsPerInch -density 72  ", CmdArgs).

media_data_url_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    Data = z_media_tag:scomp_data_url(<<"lib/images/trans.gif">>, [], Context),
    ?assertEqual(Data, <<"data:image/gif;base64,R0lGODlhAQABAJAAAAAAAAAAACH5BAEUAAAALAAAAAABAAEAAAICRAEAOw==">>).

