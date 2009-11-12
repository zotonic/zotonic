%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-03-02
%%
%% @doc Identify files, fetch metadata about an image
%% @todo Recognize more files based on magic number, think of office files etc.

%% Copyright 2009 Marc Worrell
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
    extension/1,
	guess_mime/1
]).

-include_lib("zotonic.hrl").


%% @spec identifyImageFile) -> {ok, Meta} | {error, Error}
%% @doc Caching version of identify/1. Fetches information about an image, returns width, height, type, etc.
identify(ImageFile, Context) ->
    F = fun() ->
            case m_media:identify(ImageFile, Context) of
                {ok, _Props} = Result -> Result;
                {error, _Reason} -> identify_file(ImageFile, Context)
            end
    end,
    z_depcache:memo(F, {media_identify, ImageFile}, ?DAY, [media_identify], Context).
    


%% @spec identify(ImageFile) -> {ok, PropList} | {error, Reason}
%% @doc Fetch information about an image, returns width, height, type, etc.
identify_file(ImageFile, Context) ->
    case z_notifier:first({media_identify_file, ImageFile}, Context) of
        {ok, Props} -> {ok, Props};
        undefined -> identify_file_imagemagick(ImageFile)
    end.
    

%% @spec identify(ImageFile) -> {ok, PropList} | {error, Reason}
%% @doc Try to identify the file using image magick
identify_file_imagemagick(ImageFile) ->
    CleanedImageFile = z_utils:os_escape(ImageFile),
    Result    = os:cmd("identify -quiet \"" ++ CleanedImageFile ++ "[0]\""),
    % ["test/a.jpg","JPEG","3440x2285","3440x2285+0+0","8-bit","DirectClass","2.899mb"]
    % sometimes:
    % test.jpg[0]=>test.jpg JPEG 2126x1484 2126x1484+0+0 DirectClass 8-bit 836.701kb 0.130u 0:02
    [Line1|_] = string:tokens(Result, "\r\n"),
    Words = string:tokens(Line1, " "),
    WordCount = length(Words),
    Words1 = if
        WordCount > 4 -> 
            {A,_B} = lists:split(4, Words),
            A;
        true -> 
            Words
    end,
    try 
        [_Path, Type, Dim, _Dim2] = Words1,
        [Width,Height] = string:tokens(Dim, "x"),
        {ok, [
            {width, list_to_integer(Width)},
            {height, list_to_integer(Height)},
            {mime, mime(Type)},
            {orientation, 1}
        ]}
    catch 
        _:_ ->
            ?LOG("identify of ~p failed - ~p", [CleanedImageFile, Line1]),
            {error, "unknown result from 'identify': '"++Line1++"'"}
    end.


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
%% @todo Include extra mime types when we can identify them.
extension(B) when is_binary(B) -> extension(binary_to_list(B));
extension("image/jpeg") -> ".jpg";
extension("image/gif") -> ".gif";
extension("image/tiff") -> ".tiff";
extension("image/bmp") -> ".bmp";
extension("image/png") -> ".png";
extension("application/pdf") -> ".pdf";
extension("application/postscript") -> ".ps";
extension("application/vnd.ms-excel") -> ".xls";
extension("application/msword") -> ".doc";
extension("application/vnd.ms-powerpoint") -> ".pps";
extension("audio/mpeg") -> ".mp3";
extension("video/mp4") -> ".mp4";
extension("video/mpeg") -> ".mpg";
extension("video/msvideo") -> ".avi";
extension("video/x-ms-asf") -> ".asf";
extension("video/quicktime") -> ".mov";
extension(_) -> ".bin".

%% @spec guess_mime(string()) -> string()
%% @doc  Guess the mime type of a file by the extension of its filename.
guess_mime(File) ->
    case filename:extension(File) of
	".htm" -> "text/html";
	".html" -> "text/html";
	".xhtml" -> "application/xhtml+xml";
	".xml" -> "application/xml";
	".css" -> "text/css";
	".js" -> "application/x-javascript";
	".jpg" -> "image/jpeg";
	".jpeg" -> "image/jpeg";
	".gif" -> "image/gif";
	".png" -> "image/png";
	".bmp" -> "image/bmp";
	".tiff" -> "image/tiff";
	".tif" -> "image/tiff";
	".ico" -> "image/vnd.microsoft.icon";
	".pdf" -> "application/pdf";
	".ps" -> "application/ps";
	".swf" -> "application/x-shockwave-flash";
	".zip" -> "application/zip";
	".bz2" -> "application/x-bzip2";
	".gz" -> "application/x-gzip";
	".tar" -> "application/x-tar";
	".tgz" -> "application/x-gzip";
    ".htc" -> "text/x-component";
	".txt" -> "text/plain";
	".doc" -> "application/msword";
	".xls" -> "application/vnd.ms-excel";
	".pps" -> "application/vnd.ms-powerpoint";
	".ppt" -> "application/vnd.ms-powerpoint";
	".mp3" -> "audio/mpeg";
	".mp4" -> "video/mp4";
	".mpg" -> "video/mpeg";
	".mpeg" -> "video/mpeg";
	".avi" -> "video/msvideo";
	".asf" -> "video/x-ms-asf";
	".mov" -> "video/quicktime";
	_ -> "application/octet-stream"
end.

