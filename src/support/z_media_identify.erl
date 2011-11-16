%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-03-02
%%
%% @doc Identify files, fetch metadata about an image
%% @todo Recognize more files based on magic number, think of office files etc.

%% Copyright 2009-2011 Marc Worrell, Konstantin Nikiforov
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
	identify_file/2,
	identify_file/3,
	identify_file_direct/2,
    extension/1,
	guess_mime/1,
    is_mime_compressed/1
]).

-include_lib("zotonic.hrl").


%% @spec identify(File, Context) -> {ok, Meta} | {error, Error}
%% @doc Caching version of identify/1. Fetches information about an image, returns width, height, type, etc.
identify(#upload{tmpfile=File, filename=Filename}, Context) ->
	identify(File, Filename, Context);
identify(File, Context) ->
	identify(File, File, Context).
identify(File, OriginalFilename, Context) ->
    F = fun() ->
            case m_media:identify(File, Context) of
                {ok, _Props} = Result -> Result;
                {error, _Reason} -> identify_file(File, OriginalFilename, Context)
            end
    end,
    z_depcache:memo(F, {media_identify, File}, ?DAY, [media_identify], Context).
    


%% @spec identify_file(File::filename(), Context) -> {ok, PropList} | {error, Reason}
%% @doc Fetch information about a file, returns mime, width, height, type, etc.  First checks if a module
%% has a specific identification methods.
identify_file(File, Context) ->
	identify_file(File, File, Context).
identify_file(File, OriginalFilename, Context) ->
    case z_notifier:first(#media_identify_file{filename=File}, Context) of
        {ok, Props} ->
			{ok, Props};
        undefined -> 
			identify_file_direct(File, OriginalFilename)
	end.


%% @spec identify_file_direct(File, OriginalFilename) -> {ok, PropList} | {error, Reason}
%% @doc Fetch information about a file, returns mime, width, height, type, etc.
identify_file_direct(File, OriginalFilename) ->
    {OsFamily, _} = os:type(),
	case identify_file_os(OsFamily, File, OriginalFilename) of
		{error, _} ->
			%% Last resort, give ImageMagick a try
			identify_file_imagemagick(OsFamily, File);
		{ok, Props} ->
			%% Images, pdf and ps are further investigated by ImageMagick
			case proplists:get_value(mime, Props) of
				"image/" ++ _ -> identify_file_imagemagick(OsFamily, File);
				"application/pdf" -> identify_file_imagemagick(OsFamily, File);
				"application/postscript" -> identify_file_imagemagick(OsFamily, File);
				_Mime -> {ok, Props}
			end
	end.


%% @spec identify_file_os(OsFamily::atom(), File::string(), OriginalFilename::string()) -> {ok, PropList} | {error, Reason}
%% @doc Identify the mime type of a file using the unix "file" command.
identify_file_os(win32, _File, OriginalFilename) ->
    {ok, [{mime, guess_mime(OriginalFilename)}]};

identify_file_os(unix, File, OriginalFilename) ->
    SafeFile = z_utils:os_filename(File),
    Mime = z_string:trim(os:cmd("file -b --mime-type "++SafeFile)),
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
                        _ -> {ok, [{mime, "application/vnd.ms-office"}]}
                    end;
                _ ->
                    {ok, [{mime, Mime}]}
            end
    end.


%% @spec identify_file_imagemagick(OsFamily, ImageFile) -> {ok, PropList} | {error, Reason}
%% @doc Try to identify the file using image magick
identify_file_imagemagick(OsFamily, ImageFile) ->
    CleanedImageFile = z_utils:os_filename(ImageFile ++ "[0]"),
    Result    = os:cmd("identify -quiet " ++ CleanedImageFile ++ " 2> " ++ devnull(OsFamily)),
    case Result of
        [] ->
            Err = os:cmd("identify -quiet 2>&1" ++ CleanedImageFile),
            ?LOG("identify of ~s failed:~n~s", [CleanedImageFile, Err]),
            {error, "identify error: " ++ Err};
        _ ->
            %% ["test/a.jpg","JPEG","3440x2285","3440x2285+0+0","8-bit","DirectClass","2.899mb"]
            %% sometimes:
            %% test.jpg[0]=>test.jpg JPEG 2126x1484 2126x1484+0+0 DirectClass 8-bit 836.701kb 0.130u 0:02

            %% "/tmp/ztmp-zotonic008prod@miffy.local-1321.452998.868252[0]=>/tmp/ztmp-zotonic008prod@miffy.local-1321.452998.868252 JPEG 1824x1824 1824x1824+0+0 8-bit DirectClass 1.245MB 0.000u 0:00.000"

            Line1 = hd(string:tokens(Result, "\r\n")),
            try
                Words = string:tokens(Line1, " "),
                WordCount = length(Words),
                Words1 = if
                             WordCount > 4 -> 
                                 {A,_B} = lists:split(4, Words),
                                 A;
                             true -> 
                                 Words
                         end,

                [_Path, Type, Dim, _Dim2] = Words1,
                Mime = mime(Type),
                [Width,Height] = string:tokens(Dim, "x"),
                Props1 = [{width, list_to_integer(Width)},
                          {height, list_to_integer(Height)},
                          {mime, Mime}],
                Props2 = case Mime of
                             "image/" ++ _ ->
                                 [{orientation, exif_orientation(ImageFile)} | Props1];
                             _ -> Props1
                         end,
                {ok, Props2}
            catch
                _:_ ->
                    ?LOG("identify of ~p failed - ~p", [CleanedImageFile, Line1]),
                    {error, "unknown result from 'identify': '"++Line1++"'"}
            end
    end.

devnull(win32) -> "nul";
devnull(unix)  -> "/dev/null".


%% @spec mime(String) -> MimeType
%% @doc Map the type returned by ImageMagick to a mime type
%% @todo Add more imagemagick types, check the mime types
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
mime(Type) -> "image/" ++ string:to_lower(Type).


%% @doc Return the extension for a known mime type.
extension(B) when is_binary(B) -> 
	extension(binary_to_list(B));
extension(Mime) ->
	case lists:keysearch(Mime, 2, extension_mime()) of
		{value,{Ext,_Mime}} -> Ext;
		false -> ".bin"
	end.


%% @spec guess_mime(string()) -> string()
%% @doc  Guess the mime type of a file by the extension of its filename.
guess_mime(File) when is_binary(File) ->
	guess_mime(binary_to_list(File));
guess_mime(File) ->
    case lists:keysearch(z_string:to_lower(filename:extension(File)), 1, extension_mime()) of
		{value,{_Ext,Mime}} -> 
			Mime;
		false ->
			"application/octet-stream"
	end.


% @doc Return a list of mime-type with their extension.
extension_mime() ->
	[
		% Preferred extension/mime-type mapping
		{".aiff", "audio/x-aiff"},
		{".asf", "video/x-ms-asf"},
		{".au", "audio/basic"},
		{".avi", "video/msvideo"},
		{".bin", "application/octet-stream"},
		{".bmp", "image/bmp"},
		{".bz2", "application/x-bzip2"},
		{".c", "text/x-c"},
		{".csh", "application/x-csh"},
		{".css", "text/css"},
		{".csv", "text/csv"},
		{".diff", "text/x-diff"},
		{".doc", "application/msword"},
		{".docx", "application/vnd.openxmlformats-officedocument.wordprocessingml.document"},
		{".dot", "application/x-dot"},
		{".dotx", "application/vnd.openxmlformats-officedocument.wordprocessingml.template"},
		{".dvi", "application/x-dvi"},
		{".dwg", "application/acad"},
		{".flipchart", "application/inspire"},
		{".gif", "image/gif"},
		{".gz", "application/x-gzip"},
		{".hqx", "application/mac-binhex40"},
		{".htc", "text/x-component"},
		{".html", "text/html"},
		{".ico", "image/vnd.microsoft.icon"},
		{".jar", "application/java-archive"},
		{".jpg", "image/jpeg"},
		{".js", "application/x-javascript"},
		{".json", "application/json"},
		{".latex", "application/x-latex"},
		{".manifest", "text/cache-manifest"},
		{".mdb", "application/x-msaccess"},
		{".midi", "audio/midi"},
		{".mov", "video/quicktime"},
		{".mp3", "audio/mpeg"},
		{".mp4", "video/mp4"},
		{".mpg", "video/mpeg"},
		{".mpp", "application/vnd.ms-project"},
		{".odc", "application/vnd.oasis.opendocument.chart"},
		{".odf", "application/vnd.oasis.opendocument.formula"},
		{".odg", "application/vnd.oasis.opendocument.graphics"},
		{".odi", "application/vnd.oasis.opendocument.image"},
		{".odm", "application/vnd.oasis.opendocument.text-master"},
		{".odp", "application/vnd.oasis.opendocument.presentation"},
		{".ods", "application/vnd.oasis.opendocument.spreadsheet"},
		{".odt", "application/vnd.oasis.opendocument.text"},
		{".otc", "application/vnd.oasis.opendocument.chart-template"},
		{".otf", "application/vnd.oasis.opendocument.formula-template"},
		{".otg", "application/vnd.oasis.opendocument.graphics-template"},
		{".oth", "application/vnd.oasis.opendocument.text-web"},
		{".oti", "application/vnd.oasis.opendocument.image-template"},
		{".otp", "application/vnd.oasis.opendocument.presentation-template"},
		{".ots", "application/vnd.oasis.opendocument.spreadsheet-template"},
		{".ott", "application/vnd.oasis.opendocument.text-template"},
		{".patch", "text/patch"},
		{".pdf", "application/pdf"},
		{".php", "text/x-php"},
		{".png", "image/png"},
		{".ppt", "application/vnd.ms-powerpoint"},
		{".pptx", "application/vnd.openxmlformats-officedocument.presentationml.presentation"},
		{".ps", "application/postscript"},
		{".psd", "image/vnd.adobe.photoshop"},
		{".rar", "application/x-rar"},
		{".rtf", "text/rtf"},
		{".sh", "text/x-shellscript"},
		{".sit", "application/x-stuffit"},
		{".svg", "image/svg+xml"},
		{".swf", "application/x-shockwave-flash"},
		{".tar", "application/x-tar"},
		{".tgz", "application/x-gzip+tar"},
		{".tif", "image/tiff"},
		{".tpl", "text/html"},
		{".txt", "text/plain"},
		{".wav", "audio/x-wav"},
		{".woff", "application/x-font-woff"},
		{".wmf", "application/x-msmetafile"},
		{".xhtml", "application/xhtml+xml"},
		{".xls", "application/vnd.ms-excel"},
		{".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"},
		{".xml", "application/xml"},
		{".z", "application/x-compress"},
		{".zip", "application/zip"},
		
		% Alternative mime mappings
		{".ai", "application/postscript"},
		{".aif", "audio/x-aiff"},
		{".aifc", "audio/x-aiff"},
		{".eps", "application/postscript"},
		{".erl", "text/plain"},
		{".flp", "application/inspire"},
		{".gzip", "application/x-gzip"},
		{".htm", "text/html"},
		{".jpeg", "image/jpeg"},
		{".js", "text/javascript"},
		{".js", "text/x-javascript"},
		{".mid", "audio/midi"},
		{".mpeg", "video/mpeg"},
		{".pps", "application/vnd.ms-powerpoint"},
		{".ps", "application/ps"},
		{".qt", "video/quicktime"},
		{".rtf", "application/msword"},
		{".sh", "application/x-sh"},
		{".tiff", "image/tiff"}
	].


%% Detect the exif rotation in an image and swaps width/height accordingly.
exif_orientation(InFile) ->
    %% FIXME - don't depend on external command
    case string:tokens(os:cmd("exif -m -t Orientation " ++ z_utils:os_filename(InFile))) of
        [] -> 
            1;
        [Line|_] -> 
            FirstLine = z_string:to_lower(Line),
            case [z_string:trim(X) || X <- string:tokens(FirstLine, "-")] of
                ["top", "left"] -> 1;
                ["top", "right"] -> 2;
                ["bottom", "right"] -> 3;
                ["bottom", "left"] -> 4;
                ["left", "top"] -> 5;
                ["right", "top"] -> 6;
                ["right", "bottom"] -> 7;
                ["left", "bottom"] -> 8;
                _ -> 1
            end
    end.


%% @doc Given a mime type, return whether its file contents is already compressed or not.
%% @spec is_mime_compressed(Mime::string()) -> bool()
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
