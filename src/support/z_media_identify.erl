%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2014 Marc Worrell
%%
%% @doc Identify files, fetch metadata about an image
%% @todo Recognize more files based on magic number, think of office files etc.

%% Copyright 2009-2014 Marc Worrell, Konstantin Nikiforov
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

-module(z_media_identify).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    identify/2,
	identify/3,
    identify/4,
	identify_file/2,
	identify_file/3,
	identify_file_direct/2,
    extension/1,
    extension/2,
    extension/3,
	guess_mime/1,
    is_mime_vector/1,
    is_mime_compressed/1
]).

-include_lib("zotonic.hrl").


%% @doc Caching version of identify/1. Fetches information about an image, returns width, height, type, etc.
-spec identify(#upload{}|string(), #context{}) -> {ok, Props::list()} | {error, term()}.
identify(#upload{tmpfile=File, filename=Filename}, Context) ->
	identify(File, Filename, Context);
identify(File, Context) ->
	identify(File, File, Context).

-spec identify(#upload{}|string(), string(), #context{}) -> {ok, Props::list()} | {error, term()}.
identify(File, OriginalFilename, Context) ->
    identify(File, File, OriginalFilename, Context).

identify(File, MediumFilename, OriginalFilename, Context) ->
    F = fun() ->
            case m_media:identify(MediumFilename, Context) of
                {ok, _Props} = Result -> Result;
                {error, _Reason} -> identify_file(File, OriginalFilename, Context)
            end
    end,
    z_depcache:memo(F, {media_identify, MediumFilename}, ?DAY, [media_identify], Context).



%% @doc Fetch information about a file, returns mime, width, height, type, etc.  First checks if a module
%% has a specific identification methods.
-spec identify_file(File::string(), #context{}) -> {ok, Props::list()} | {error, term()}.
identify_file(File, Context) ->
	identify_file(File, File, Context).

-spec identify_file(File::string(), OriginalFilename::string(), #context{}) -> {ok, Props::list()} | {error, term()}.
identify_file(File, OriginalFilename, Context) ->
    Extension = maybe_extension(File, OriginalFilename),
    case z_notifier:first(#media_identify_file{filename=File, original_filename=OriginalFilename, extension=Extension}, Context) of
        {ok, Props} ->
			{ok, Props};
        undefined ->
            identify_file_direct(File, OriginalFilename)
	end.

maybe_extension(File, undefined) ->
    maybe_extension(File);
maybe_extension(_File, OriginalFilename) ->
    maybe_extension(OriginalFilename).

maybe_extension(undefined) ->
    "";
maybe_extension(Filename) ->
    z_convert:to_list(z_string:to_lower(filename:extension(Filename))).

%% @doc Fetch information about a file, returns mime, width, height, type, etc.
-spec identify_file_direct(File::string(), OriginalFilename::string()) -> {ok, Props::list()} | {error, term()}.
identify_file_direct(File, OriginalFilename) ->
    check_acceptable(File, maybe_identify_extension(identify_file_direct_1(File, OriginalFilename), OriginalFilename)).

identify_file_direct_1(File, OriginalFilename) ->
    {OsFamily, _} = os:type(),
	case identify_file_os(OsFamily, File, OriginalFilename) of
		{error, _} ->
			%% Last resort, give ImageMagick a try
			identify_file_imagemagick(OsFamily, File, undefined);
		{ok, Props} ->
			%% Images, pdf and ps are further investigated by ImageMagick
			case proplists:get_value(mime, Props) of
				"image/" ++ _ = M -> identify_file_imagemagick(OsFamily, File, M);
				"application/pdf" = M -> identify_file_imagemagick(OsFamily, File, M);
				"application/postscript" = M -> identify_file_imagemagick(OsFamily, File, M);
				_Mime -> {ok, Props}
			end
	end.

check_acceptable(_File, {error, _} = Error) ->
    Error;
check_acceptable(File, {ok, Props}) ->
    case z_media_sanitize:is_file_acceptable(File, Props) of
        false -> {ok, [{mime, "application/octet-stream"}]};
        true -> {ok, Props}
    end.

maybe_identify_extension({error, "identify error: "++_}, OriginalFilename) ->
    {ok, [ {mime, guess_mime(OriginalFilename)} ]};
maybe_identify_extension({ok, [{mime,"application/octet-stream"}]}, OriginalFilename) ->
    {ok, [ {mime, guess_mime(OriginalFilename)} ]};
maybe_identify_extension(Result, _OriginalFilename) ->
    Result.

%% @doc Identify the mime type of a file using the unix "file" command.
-spec identify_file_os(win32|unix, File::string(), OriginalFilename::string()) -> {ok, Props::list()} | {error, term()}.
identify_file_os(win32, _File, OriginalFilename) ->
    {ok, [{mime, guess_mime(OriginalFilename)}]};
identify_file_os(unix, File, OriginalFilename) ->
    identify_file_unix(os:find_executable("file"), File, OriginalFilename).

identify_file_unix(false, _File, _OriginalFilename) ->
    lager:error("Please install 'file' for identifying the type of uploaded files."),
    {error, "'file' not installed"};
identify_file_unix(Cmd, File, OriginalFilename) ->
    Mime = z_string:trim(
                os:cmd(z_utils:os_filename(Cmd)
                        ++" -b --mime-type "
                        ++ z_utils:os_filename(File))),
    case re:run(Mime, "^[a-zA-Z0-9_\\-\\.]+/[a-zA-Z0-9\\.\\-_]+$") of
        nomatch ->
            case Mime of
                "CDF V2 Document, corrupt:" ++ _ ->
                    % Probably just a semi-illegal variation on a MS Office file, use the extension
                    case guess_mime(OriginalFilename) of
                        "application/msword" -> {ok, [{mime, "application/msword"}]};
                        "application/vnd.ms-excel" -> {ok, [{mime, "application/vnd.ms-excel"}]};
                        "application/vnd.ms-powerpoint" -> {ok, [{mime, "application/vnd.ms-powerpoint"}]};
                        _ -> {error, Mime}
                    end;
                _ ->
                    {error, Mime}
            end;
        {match, _} ->
            case Mime of
                "text/x-c" ->
                    %% "file" does a lousy job recognizing files with curly braces in them.
                    Mime2 = case guess_mime(OriginalFilename) of
                        "text/" ++ _ = MimeFilename -> MimeFilename;
                        "application/x-" ++ _ = MimeFilename -> MimeFilename;
                        "application/json" -> "application/json";
                        _ -> "text/plain"
                    end,
                    {ok, [{mime, Mime2}]};
                "application/x-gzip" ->
                    %% Special case for the often used extension ".tgz" instead of ".tar.gz"
                    case filename:extension(OriginalFilename) of
                        ".tgz" -> {ok, [{mime, "application/x-gzip+tar"}]};
                        _ -> {ok, [{mime, "application/x-gzip"}]}
                    end;
                "application/zip" ->
                    %% Special case for zip'ed office files
                    case guess_mime(OriginalFilename) of
                        "application/vnd.openxmlformats-officedocument." ++ _ = OfficeMime ->
                            {ok, [{mime, OfficeMime}]};
                        _ ->
                            {ok, [{mime, "application/zip"}]}
                    end;
                "application/ogg" ->
                    % The file utility does some miss-guessing
                    case guess_mime(OriginalFilename) of
                        "video/ogg" -> {ok, [{mime, "video/ogg"}]};
                        "audio/ogg" -> {ok, [{mime, "audio/ogg"}]};
                        _ -> {ok, [{mime, "application/ogg"}]}
                    end;
                "application/octet-stream" ->
                    % The file utility does some miss-guessing
                    case guess_mime(OriginalFilename) of
                        "text/csv" -> {ok, [{mime, "text/csv"}]};
                        "application/vnd.oasis.opendocument." ++ _ = ODF -> {ok, [{mime, ODF}]};
                        "application/inspire" -> {ok, [{mime, "application/inspire"}]};
                        "video/mpeg" -> {ok, [{mime, "video/mpeg"}]};
                        "audio/mpeg" -> {ok, [{mime, "audio/mpeg"}]};
                        _ -> {ok, [{mime, "application/octet-stream"}]}
                    end;
                "application/vnd.ms-office" ->
                    % Generic ms-office mime type, check if the filename is more specific
                    case guess_mime(OriginalFilename) of
                        "application/vnd.ms" ++ _ = M -> {ok, [{mime,M}]};
                        "application/msword" -> {ok, [{mime,"application/msword"}]};
                        "application/vnd.visio" -> {ok, [{mime,"application/vnd.visio"}]};
                        _ -> {ok, [{mime, "application/vnd.ms-office"}]}
                    end;
                "application/vnd.ms-excel" = Excel ->
                    case guess_mime(OriginalFilename) of
                        "application/vnd.openxmlformats" ++ _ = M -> {ok, [{mime,M}]};
                        _ -> {ok, [{mime, Excel}]}
                    end;
                "audio/x-wav" ->
                    case guess_mime(OriginalFilename) of
                        "audio/" ++ _ = M -> {ok, [{mime,M}]};
                        _ -> {ok, [{mime, "audio/x-wav"}]}
                    end;
                "video/x-ms-asf" ->
                    case guess_mime(OriginalFilename) of
                        "audio/" ++ _ = M -> {ok, [{mime,M}]};
                        _ -> {ok, [{mime, "video/x-ms-asf"}]}
                    end;
                _ ->
                    {ok, [{mime, Mime}]}
            end
    end.


%% @doc Try to identify the file using image magick
-spec identify_file_imagemagick(win32|unix, Filename::string(), MimeFile::string()|undefined) -> {ok, Props::list()} | {error, term()}.
identify_file_imagemagick(OsFamily, ImageFile, MimeFile) ->
    identify_file_imagemagick_1(os:find_executable("identify"), OsFamily, ImageFile, MimeFile).

identify_file_imagemagick_1(false, _OsFamily, _ImageFile, _MimeFile) ->
    lager:error("Please install ImageMagick 'identify' for identifying the type of uploaded files."),
    {error, "'identify' not installed"};
identify_file_imagemagick_1(Cmd, OsFamily, ImageFile, MimeFile) ->
    CleanedImageFile = z_utils:os_filename(ImageFile ++ "[0]"),
    CmdOutput = os:cmd(z_utils:os_filename(Cmd)
                       ++" -quiet "
                       ++CleanedImageFile
                       ++" 2> " ++ devnull(OsFamily)),
    Lines = lists:dropwhile(
                    fun
                        ("Warning:" ++ _) -> true;
                        ("Can't" ++ _) -> true;
                        ("   ****" ++ _) -> true;
                        (_) -> false
                    end,
                    string:tokens(CmdOutput, "\n")),
    case Lines of
        [] ->
            Err = os:cmd(z_utils:os_filename(Cmd)
                         ++" -quiet "
                         ++CleanedImageFile
                         ++" 2>&1"),
            lager:info("identify of ~s failed:~n~s", [CleanedImageFile, Err]),
            {error, "identify error: " ++ Err};
        [Result|_] ->
            %% ["test/a.jpg","JPEG","3440x2285","3440x2285+0+0","8-bit","DirectClass","2.899mb"]
            %% sometimes:
            %% test.jpg[0]=>test.jpg JPEG 2126x1484 2126x1484+0+0 DirectClass 8-bit 836.701kb 0.130u 0:02

            %% "/tmp/ztmp-zotonic008prod@miffy.local-1321.452998.868252[0]=>/tmp/ztmp-zotonic008prod@miffy.local-1321.452998.868252 JPEG 1824x1824 1824x1824+0+0 8-bit DirectClass 1.245MB 0.000u 0:00.000"
            try
                [_Path, Type, Dim, _Dim2 | _] = string:tokens(Result, " "),
                Mime = mime(Type, MimeFile),
                [Width,Height] = string:tokens(Dim, "x"),
                {W1,H1} = maybe_sizeup(Mime, list_to_integer(Width), list_to_integer(Height)),
                Props1 = [{width, W1},
                          {height, H1},
                          {mime, Mime}],
                Props2 = case Mime of
                             "image/" ++ _ ->
                                 Exif = exif(ImageFile),
                                 Orientation = exif_orientation(Exif),
                                 Orientation1 = correct_orientation(Orientation, Exif, W1, H1),
                                 [
                                    {orientation, Orientation1},
                                    {exif, Exif},
                                    {subject_point, exif_subject_point(Exif, Orientation, W1, H1)}
                                  | Props1
                                 ];
                             _ ->
                                Props1
                         end,
                {ok, Props2}
            catch
                X:B ->
                    StackTrace= erlang:get_stacktrace(),
                    lager:info("identify of ~p failed - ~p with ~p:~p in ~p", 
                              [CleanedImageFile, CmdOutput, X, B, StackTrace]),
                    {error, "unknown result from 'identify': '"++CmdOutput++"'"}
            end
    end.

%% @doc Prevent unneeded 'extents' for vector based inputs.
maybe_sizeup(Mime, W, H) ->
    case is_mime_vector(Mime) of
        true -> {W*2, H*2};
        false -> {W,H}
    end.

is_mime_vector("application/pdf") -> true;
is_mime_vector("application/postscript") -> true;
is_mime_vector("image/svg+xml") -> true;
is_mime_vector(<<"application/pdf">>) -> true;
is_mime_vector(<<"application/postscript">>) -> true;
is_mime_vector(<<"image/svg+xml">>) -> true;
is_mime_vector(_) -> false.


-spec devnull(win32|unix) -> string().
devnull(win32) -> "nul";
devnull(unix)  -> "/dev/null".


-spec mime(string(), string()|undefined) -> string().
%% @doc ImageMagick identify can identify PDF/PS files as PBM
mime("PBM", MimeFile) when is_list(MimeFile) -> MimeFile;
mime(Type, _) -> mime(Type).

%% @doc Map the type returned by ImageMagick to a mime type
%% @todo Add more imagemagick types, check the mime types
-spec mime(string()) -> string().
mime("JPEG") -> "image/jpeg";
mime("GIF") -> "image/gif";
mime("TIFF") -> "image/tiff";
mime("BMP") -> "image/bmp";
mime("PDF") -> "application/pdf";
mime("PS") -> "application/postscript";
mime("PS2") -> "application/postscript";
mime("PS3") -> "application/postscript";
mime("PNG") -> "image/png";
mime("PNG8") -> "image/png";
mime("PNG24") -> "image/png";
mime("PNG32") -> "image/png";
mime("SVG") -> "image/svg+xml";
mime(Type) -> "image/" ++ string:to_lower(Type).



%% @doc Return the extension for a known mime type (eg. ".mov").
-spec extension(string()|binary()) -> string().
extension(Mime) -> extension(Mime, undefined).

%% @doc Return the extension for a known mime type (eg. ".mov"). When
%% multiple extensions are found for the given mime type, returns the
%% one that is given as the preferred extension. Otherwise, it returns
%% the first extension.
-spec extension(string()|binary(), string()|binary()|undefined, #context{}) -> string().
extension(Mime, PreferExtension, Context) ->
    case z_notifier:first(#media_identify_extension{mime=maybe_binary(Mime), preferred=maybe_binary(PreferExtension)}, Context) of
        undefined ->
            extension(Mime, PreferExtension);
        Extension ->
            z_convert:to_list(Extension)
    end.

maybe_binary(undefined) -> undefined;
maybe_binary(L) -> z_convert:to_binary(L).

-spec extension(string()|binary(), string()|binary()|undefined) -> string().
extension("image/jpeg", _PreferExtension) -> ".jpg";
extension(<<"image/jpeg">>, _PreferExtension) -> ".jpg";
extension("application/vnd.ms-excel", _) -> ".xls";
extension(<<"application/vnd.ms-excel">>, _) -> ".xls";
extension("text/plain", _PreferExtension) -> ".txt";
extension(<<"text/plain">>, _PreferExtension) -> ".txt";
extension(Mime, PreferExtension) ->
    Extensions = mimetypes:extensions(z_convert:to_binary(Mime)),
    case PreferExtension of
        undefined ->
            first_extension(Extensions);
        _ ->
            %% convert prefer extension to something that mimetypes likes
            Ext1 = z_convert:to_binary(z_string:to_lower(PreferExtension)),
            Ext2 = case Ext1 of
                       <<$.,Rest/binary>> -> Rest;
                       _ -> Ext1
                   end,
            case lists:member(Ext2, Extensions) of
                true ->
                    [$. | z_convert:to_list(Ext2)];
                false ->
                    first_extension(Extensions)
            end
    end.


first_extension([]) ->
    ".bin";
first_extension(Extensions) ->
    [$. | z_convert:to_list(hd(Extensions))].


%% @spec guess_mime(string()) -> string()
%% @doc  Guess the mime type of a file by the extension of its filename.
-spec guess_mime(string() | binary()) -> string().
guess_mime(File) ->
	case mimetypes:filename(z_convert:to_binary(z_string:to_lower(File))) of
		[Mime|_] -> z_convert:to_list(Mime);
		[] -> "application/octet-stream"
	end.


% Fetch the EXIF information from the file, we remove the maker_note as it can be huge
exif(File) ->
    case exif:read(File) of
        {ok, Dict} ->
            List = dict:to_list(Dict),
            proplists:delete(maker_note, List);
        {error, _} ->
            []
    end.

%% Detect the exif rotation in an image and swaps width/height accordingly.
-spec exif_orientation(list()) -> 1|2|3|4|5|6|7|8.
exif_orientation(undefined) ->
    1;
exif_orientation(Exif) when is_list(Exif) ->
    case proplists:get_value(orientation, Exif) of
        <<"Top-left">> -> 1;
        <<"Top-right">> -> 2;
        <<"Bottom-right">> -> 3;
        <<"Bottom-left">> -> 4;
        <<"Left-top">> -> 5;
        <<"Right-top">> -> 6;
        <<"Right-bottom">> -> 7;
        <<"Left-bottom">> -> 8;
        _ -> 1
    end.

%% See also http://www.awaresystems.be/imaging/tiff/tifftags/privateifd/exif/subjectarea.html
exif_subject_point(Exif, Orientation, Width, Height) ->
    Point = extract_subject_point(Exif),
    Point1 = maybe_resize_point(Point, Exif, Width, Height),
    maybe_rotate(Orientation, Point1, Width, Height).

extract_subject_point(undefind) ->
    undefined;
extract_subject_point(Exif) ->
    case proplists:get_value(subject_area, Exif) of
        [X, Y] -> {X, Y};
        [X, Y, _Radius] -> {X, Y};
        [X, Y, _W, _H] -> {X, Y};
        _ -> undefined
    end.

maybe_resize_point(undefined, _Exif, _Width, _Height) ->
    undefined;
maybe_resize_point({X, Y}, Exif, Width, Height) ->
    ExifWidth = proplists:get_value(pixel_x_dimension, Exif),
    ExifHeight = proplists:get_value(pixel_y_dimension, Exif),
    case is_integer(ExifWidth) andalso is_integer(ExifHeight) of
        true when ExifWidth =:= Width, ExifHeight =:= Height ->
            {X, Y};
        true when ExifWidth =:= 0; ExifHeight =:= 0 ->
            {X, Y};
        true ->
            {round(X*Width/ExifWidth), round(Y*Height/ExifHeight)};
        false ->
            {X, Y}
    end.


maybe_rotate(_, undefined, _W, _H) -> undefined;
maybe_rotate(1, {X, Y}, _W, _H) -> {X, Y};
maybe_rotate(2, {X, Y}, _W, H) -> {X, H-Y}; % flip
maybe_rotate(3, {X, Y}, W, H) -> {W-X, H-Y}; % rotate 180
maybe_rotate(4, {X, Y}, W, _H) -> {W-X, Y}; % flop
maybe_rotate(5, {X, Y}, W, H) -> {W-X, H-Y}; % transpose
maybe_rotate(6, {X, Y}, _W, H) -> {H-Y, X}; % rotate 90
maybe_rotate(7, {X, Y}, W, H) -> {W-X, H-Y}; % transverse
maybe_rotate(8, {X, Y}, W, _H) -> {Y, W-X}. % rotate 270


%% @doc Check that the orientation makes sense given the width/height of the image
correct_orientation(1, _Exif, _Width, _Height) ->
    1;
correct_orientation(Orientation, Exif, Width, Height) ->
    IsLandscape = Width > Height,
    ExifWidth = proplists:get_value(pixel_x_dimension, Exif),
    ExifHeight = proplists:get_value(pixel_y_dimension, Exif),
    case is_integer(ExifWidth) andalso is_integer(ExifHeight) of
        true when IsLandscape, ExifWidth < ExifHeight ->
            % The image is landscape, but the exif describes a portrait orientated image
            % We assume that the rotation has been done already
            1;
        true when not IsLandscape, ExifWidth > ExifHeight ->
            % The image is portrait, but the exif describes a landscape orientated image.
            % We assume that the rotation has been done already
            1;
        _ ->
            Orientation
    end.


%% @doc Given a mime type, return whether its file contents is already compressed or not.
-spec is_mime_compressed(string()) -> boolean().
is_mime_compressed("text/"++_)                               -> false;
is_mime_compressed("image/svgz"++_)                          -> true;
is_mime_compressed("image/svg"++_)                           -> false;
is_mime_compressed("image/"++_)                              -> true;
is_mime_compressed("video/"++_)                              -> true;
is_mime_compressed("audio/x-wav")                            -> false;
is_mime_compressed("audio/"++_)                              -> true;
is_mime_compressed("application/x-compres"++_)               -> true;
is_mime_compressed("application/zip")                        -> true;
is_mime_compressed("application/x-gz"++_)                    -> true;
is_mime_compressed("application/x-rar")                      -> true;
is_mime_compressed("application/x-bzip2")                    -> true;
is_mime_compressed("application/x-font-woff")                -> true;
is_mime_compressed("application/vnd.oasis.opendocument."++_) -> true;
is_mime_compressed("application/vnd.openxml"++_)             -> true;
is_mime_compressed("application/x-shockwave-flash")          -> true;
is_mime_compressed(_)                                        -> false.
