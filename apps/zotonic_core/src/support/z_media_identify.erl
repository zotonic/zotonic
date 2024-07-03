%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Identify files, fetch metadata about an image
%% @end

%% Copyright 2009-2024 Marc Worrell, Konstantin Nikiforov
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

-type optional_filename() :: undefined | file:filename_all().
-type filename_extension() :: binary().
-type mime_type() :: binary().
-type os_family() :: win32 | unix.
-type media_info() :: map().

-export_type([
    media_info/0
]).

%% @doc Caching version of identify/3. Fetches information about an image, returns width, height, type, etc.
-spec identify( #upload{} | file:filename_all(), z:context() ) -> {ok, media_info()} | {error, term()}.
identify(#upload{tmpfile=undefined, data=Data, filename=Filename}, Context) when is_binary(Data) ->
    TmpFile = z_tempfile:new(),
    case file:write_file(TmpFile, Data) of
        ok ->
            Result = identify_file(TmpFile, Filename, Context),
            file:delete(TmpFile),
            Result;
        {error, _} = Error ->
            file:delete(TmpFile),
            ?LOG_WARNING(#{
                text => <<"z_media_identify: could not write temporary file">>,
                in => zotonic_core,
                size => size(Data),
                file => TmpFile
            }),
            Error
    end;
identify(#upload{tmpfile=File, filename=Filename}, Context) ->
    % Don't cache identify results for uploaded files
    identify_file(File, Filename, Context);
identify(File, Context) ->
    identify(File, File, Context).

-spec identify(#upload{}|file:filename_all(), optional_filename(), z:context()) -> {ok, media_info()} | {error, term()}.
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
-spec identify_file( file:filename_all(), z:context() ) -> {ok, media_info()} | {error, term()}.
identify_file(File, Context) ->
    identify_file(File, File, Context).

-spec identify_file( file:filename_all(), optional_filename(), z:context() ) -> {ok, media_info()} | {error, term()}.
identify_file(File, OriginalFilename, Context) ->
    Extension = maybe_extension(File, OriginalFilename),
    case z_notifier:first(
            #media_identify_file{
                filename = File,
                original_filename = OriginalFilename,
                extension = Extension
            },
            Context)
    of
        {ok, Props} ->
            {ok, Props};
        undefined ->
            identify_file_direct(File, OriginalFilename)
    end.

-spec maybe_extension(file:filename_all(), optional_filename()) -> filename_extension().
maybe_extension(File, undefined) ->
    maybe_extension(File);
maybe_extension(_File, OriginalFilename) ->
    maybe_extension(OriginalFilename).

-spec maybe_extension( optional_filename() ) -> filename_extension().
maybe_extension(undefined) ->
    <<>>;
maybe_extension(Filename) ->
    z_convert:to_binary(z_string:to_lower(filename:extension(Filename))).

%% @doc Fetch information about a file, returns mime, width, height, type, etc.
-spec identify_file_direct( file:filename_all(), optional_filename() ) -> {ok, media_info()} | {error, term()}.
identify_file_direct(File, OriginalFilename) ->
    check_acceptable(File, maybe_identify_extension(identify_file_direct_1(File, OriginalFilename), OriginalFilename)).

%% Images, pdf and ps are further investigated by ImageMagick
identify_file_direct_1(File, OriginalFilename) ->
    OsFamily = os_family(),
    case identify_file_os(OsFamily, File, OriginalFilename) of
        {error, _} ->
            %% Last resort, give ImageMagick a try
            identify_file_imagemagick(OsFamily, File, undefined);
        {ok, #{ <<"mime">> := <<"image/", _/binary>> = Mime }} ->
            identify_file_imagemagick(OsFamily, File, Mime);
        {ok, #{ <<"mime">> := <<"application/pdf">> = Mime }} ->
            identify_file_imagemagick(OsFamily, File, Mime);
        {ok, #{ <<"mime">> := <<"application/postscript">> = Mime }} ->
            identify_file_imagemagick(OsFamily, File, Mime);
        {ok, Props} = OK when is_map(Props) ->
            OK
    end.

-spec os_family() -> os_family().
os_family() ->
    {OsFamily, _} = os:type(),
    OsFamily.

check_acceptable(_File, {error, _} = Error) ->
    Error;
check_acceptable(File, {ok, Props}) ->
    case z_media_sanitize:is_file_acceptable(File, Props) of
        false -> {ok, #{ <<"mime">> => <<"application/octet-stream">> }};
        true -> {ok, Props}
    end.

maybe_identify_extension({ok, #{ <<"mime">> := <<"application/octet-stream">> }}, OriginalFilename) ->
    {ok, #{ <<"mime">> => guess_mime(OriginalFilename) }};
maybe_identify_extension({error, identify}, OriginalFilename) ->
    {ok, #{ <<"mime">> => guess_mime(OriginalFilename) }};
maybe_identify_extension(Result, _OriginalFilename) ->
    Result.

%% @doc Identify the mime type of a file using the unix "file" command.
-spec identify_file_os(win32|unix, File :: file:filename_all(), OriginalFilename :: file:filename_all()) ->
    {ok, media_info()} | {error, term()}.
identify_file_os(win32, _File, OriginalFilename) ->
    {ok, #{ <<"mime">> => guess_mime(OriginalFilename)}};
identify_file_os(unix, File, OriginalFilename) ->
    identify_file_unix(os:find_executable("file"), File, OriginalFilename).

identify_file_unix(false, _File, _OriginalFilename) ->
    ?LOG_ERROR("Please install 'file' for identifying the type of uploaded files."),
    {error, no_file_cmd};
identify_file_unix(Cmd, File, OriginalFilename) ->
    CmdLine = unicode:characters_to_list([
        z_filelib:os_filename(Cmd),
        " -b --mime-type ",
        z_filelib:os_filename(File)
    ]),
    Mime = z_string:trim( unicode:characters_to_binary( os:cmd( CmdLine ) ) ),
    case re:run(Mime, "^[a-zA-Z0-9_\\-\\.]+/[a-zA-Z0-9\\.\\-_]+$") of
        nomatch ->
            case Mime of
                <<"CDF V2 Document, corrupt:", _/binary>> ->
                    % Probably just a semi-illegal variation on a MS Office file, use the extension
                    case guess_mime(OriginalFilename) of
                        <<"application/msword">> -> {ok, #{ <<"mime">> => <<"application/msword">>}};
                        <<"application/vnd.ms-excel">> -> {ok, #{ <<"mime">> => <<"application/vnd.ms-excel">>}};
                        <<"application/vnd.ms-powerpoint">> -> {ok, #{ <<"mime">> => <<"application/vnd.ms-powerpoint">>}};
                        _ -> {error, Mime}
                    end;
                _ ->
                    {error, Mime}
            end;
        {match, _} ->
            case Mime of
                <<"text/x-c">> ->
                    %% "file" does a lousy job recognizing files with curly braces in them.
                    MXc = case guess_mime(OriginalFilename) of
                        <<"text/csv">> ->
                            case z_csv_parser:inspect_file(File) of
                                {ok, _Hs, _Sep} -> <<"text/csv">>;
                                {error, _} -> <<"text/plain">>
                            end;
                        <<"text/", _/binary>> = MimeFilename -> MimeFilename;
                        <<"application/x-", _/binary>> = MimeFilename -> MimeFilename;
                        <<"application/json">> -> <<"application/json">>;
                        _ -> <<"text/plain">>
                    end,
                    {ok, #{ <<"mime">> => MXc}};
                <<"text/plain">> ->
                    MText = case guess_mime(OriginalFilename) of
                        <<"text/csv">> ->
                            case z_csv_parser:inspect_file(File) of
                                {ok, _Hs, _Sep} -> <<"text/csv">>;
                                {error, _} -> <<"text/plain">>
                            end;
                        _ ->
                            <<"text/plain">>
                    end,
                    {ok, #{ <<"mime">> => MText }};
                <<"application/x-gzip">> ->
                    %% Special case for the often used extension ".tgz" instead of ".tar.gz"
                    MGZip = case filename:extension(OriginalFilename) of
                        ".tgz" -> <<"application/x-gzip+tar">>;
                        <<".tgz">> -> <<"application/x-gzip+tar">>;
                        _ -> <<"application/x-gzip">>
                    end,
                    {ok, #{ <<"mime">> => MGZip }};
                <<"application/zip">> ->
                    %% Special case for zip'ed office files
                    MZip = case guess_mime(OriginalFilename) of
                        <<"application/vnd.openxmlformats-officedocument.", _/binary>> = OfficeMime ->
                            OfficeMime;
                        _ ->
                            <<"application/zip">>
                    end,
                    {ok, #{ <<"mime">> => MZip }};
                <<"application/ogg">> ->
                    % The file utility does some miss-guessing
                    MOgg = case guess_mime(OriginalFilename) of
                        <<"video/ogg">> -> <<"video/ogg">>;
                        <<"audio/ogg">> -> <<"audio/ogg">>;
                        _ -> <<"application/ogg">>
                    end,
                    {ok, #{ <<"mime">> => MOgg }};
                <<"application/octet-stream">> ->
                    % The file utility does some miss-guessing
                    MOctet = case guess_mime(OriginalFilename) of
                        <<"text/csv">> -> <<"text/csv">>;
                        <<"application/vnd.oasis.opendocument.", _/binary>> = ODF -> ODF;
                        <<"application/inspire">> -> <<"application/inspire">>;
                        <<"video/mpeg">> -> <<"video/mpeg">>;
                        <<"audio/mpeg">> -> <<"audio/mpeg">>;
                        _ ->
                            case identify_magicnumber(File) of
                                {ok, MagicNumberMime} -> MagicNumberMime;
                                {error, _} -> <<"application/octet-stream">>
                            end
                    end,
                    {ok, #{ <<"mime">> => MOctet }};
                <<"application/vnd.ms-office">> ->
                    % Generic ms-office mime type, check if the filename is more specific
                    MOffice = case guess_mime(OriginalFilename) of
                        <<"application/vnd.ms", _/binary>> = M -> M;
                        <<"application/msword">> -> <<"application/msword">>;
                        <<"application/vnd.visio">> -> <<"application/vnd.visio">>;
                        _ -> <<"application/vnd.ms-office">>
                    end,
                    {ok, #{ <<"mime">> => MOffice }};
                <<"application/vnd.ms-excel">> = Excel ->
                    MExcel = case guess_mime(OriginalFilename) of
                        <<"application/vnd.openxmlformats", _/binary>> = M -> M;
                        _ -> Excel
                    end,
                    {ok, #{ <<"mime">> => MExcel }};
                Wav when Wav =:= <<"audio/x-wav">>; Wav =:= <<"audio/wav">> ->
                    MWav = case guess_mime(OriginalFilename) of
                        <<"audio/", _/binary>> = M -> M;
                        _ -> <<"audio/wav">>
                    end,
                    {ok, #{ <<"mime">> => MWav }};
                <<"audio/x-", _/binary>> ->
                    MAudio = case guess_mime(OriginalFilename) of
                        <<"audio/", _/binary>> = M -> M;
                        _ -> Mime
                    end,
                    {ok, #{ <<"mime">> => MAudio }};
                <<"video/x-ms-asf">> ->
                    MAsf = case guess_mime(OriginalFilename) of
                        <<"audio/", _/binary>> = M -> M;
                        _ -> <<"video/x-ms-asf">>
                    end,
                    {ok, #{ <<"mime">> => MAsf }};
                <<"video/mp4">> ->
                    MMP4 = case guess_mime(OriginalFilename) of
                        <<"audio/", _/binary>> = M -> M;
                        _ -> <<"video/mp4">>
                    end,
                    {ok, #{ <<"mime">> => MMP4 }};
                _ ->
                    {ok, #{ <<"mime">> => Mime }}
            end
    end.


%% @doc Try to identify the file using image magick
-spec identify_file_imagemagick(os_family(), file:filename_all(), undefined | mime_type()) -> {ok, media_info()} | {error, term()}.
identify_file_imagemagick(OsFamily, ImageFile, MimeFile) ->
    identify_file_imagemagick_1(os:find_executable("identify"), OsFamily, ImageFile, MimeFile).

identify_file_imagemagick_1(false, _OsFamily, _ImageFile, _MimeFile) ->
    ?LOG_ERROR("Please install ImageMagick 'identify' for identifying the type of uploaded files."),
    {error, "'identify' not installed"};
identify_file_imagemagick_1(Cmd, OsFamily, ImageFile, MimeTypeFromFile) ->
    CleanedImageFile = z_filelib:os_filename(z_convert:to_list(ImageFile) ++ "[0]"),
    CmdOutput = os:cmd(z_filelib:os_filename(Cmd)
                       ++ " -quiet "
                       ++ z_convert:to_list(CleanedImageFile)
                       ++ " 2> " ++ devnull(OsFamily)),
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
            Err = os:cmd(z_filelib:os_filename(Cmd)
                         ++ " -quiet "
                         ++ z_convert:to_list(CleanedImageFile)
                         ++ " 2>&1"),
            ?LOG_NOTICE(#{
                text => <<"identify of file failed">>,
                in => zotonic_core,
                file => CleanedImageFile,
                result => error,
                output => Err
            }),
            {error, identify};
        [Result|_] ->
            %% ["test/a.jpg","JPEG","3440x2285","3440x2285+0+0","8-bit","DirectClass","2.899mb"]
            %% sometimes:
            %% test.jpg[0]=>test.jpg JPEG 2126x1484 2126x1484+0+0 DirectClass 8-bit 836.701kb 0.130u 0:02

            %% "/tmp/ztmp-zotonic008prod@miffy.local-1321.452998.868252[0]=>/tmp/ztmp-zotonic008prod@miffy.local-1321.452998.868252 JPEG 1824x1824 1824x1824+0+0 8-bit DirectClass 1.245MB 0.000u 0:00.000"
            try
                Result1 = z_string:trim( lists:last( binary:split( z_convert:to_binary(Result), z_convert:to_binary(ImageFile), [ global ] ) ) ),
                [Type, Dim, _Dim2 | _] = binary:split(Result1, <<" ">>, [ global ]),
                Mime = im_mime(Type, MimeTypeFromFile),
                [Width,Height] = binary:split( hd( binary:split(Dim, <<"=>">>) ), <<"x">>),
                {W1,H1} = maybe_size_correct(Mime, binary_to_integer(Width), binary_to_integer(Height)),
                Props1 = #{
                    <<"width">> => W1,
                    <<"height">> => H1,
                    <<"mime">> => Mime
                },
                Props2 = case Mime of
                    <<"image/", _/binary>> ->
                        Exif = exif(ImageFile),
                        Orientation = exif_orientation(Exif),
                        Orientation1 = correct_orientation(Orientation, Exif, W1, H1),
                        Props1#{
                           <<"orientation">> => Orientation1,
                           <<"exif">> => Exif,
                           <<"subject_point">> => exif_subject_point(Exif, Orientation, W1, H1)
                        };
                    _ ->
                       Props1
                end,
                Props3 = maybe_add_frame_count(Props2, ImageFile),
                {ok, Props3}
            catch
                X:B:Stacktrace ->
                    ?LOG_WARNING(#{
                        text => <<"identify of file failed">>,
                        in => zotonic_core,
                        file => CleanedImageFile,
                        result => X,
                        reason => B,
                        output => CmdOutput,
                        stack => Stacktrace
                    }),
                    {error, identify}
            end
    end.

%% @doc For (animated) GIFs, add the frame count.
maybe_add_frame_count(#{ <<"mime">> := <<"image/gif">> } = Props, Filename) ->
    Props#{
        <<"frame_count">> => z_media_gif:frame_count_file(Filename)
    };
maybe_add_frame_count(Props, _Filename) ->
    Props.


%% @doc Prevent unneeded 'extents' for vector based inputs.
maybe_size_correct(Mime, W, H) when W < 3000, H < 3000 ->
    case is_mime_vector(Mime) of
        true -> {W*2, H*2};
        false -> {W,H}
    end;
maybe_size_correct(Mime, W, H) when W > 6000; H > 6000 ->
    case is_mime_vector(Mime) of
        true -> maybe_size_correct(Mime, W div 2, H div 2);
        false -> {W,H}
    end;
maybe_size_correct(_Mime, W, H) ->
    {W, H}.

-spec is_mime_vector( string() | mime_type() ) -> boolean().
is_mime_vector(<<"application/pdf">>) -> true;
is_mime_vector(<<"application/postscript">>) -> true;
is_mime_vector(<<"image/svg+xml">>) -> true;
is_mime_vector(Mime) when is_binary(Mime) -> false;
is_mime_vector(Mime) -> is_mime_vector( z_convert:to_binary(Mime) ).


-spec devnull(win32|unix) -> string().
devnull(win32) -> "nul";
devnull(unix)  -> "/dev/null".


%% @doc ImageMagick identify can identify PDF files as:
%% - PBM which is a known problem of IM 6.8.9 (used on Ubuntu 16)
%% - AI see https://github.com/ImageMagick/ImageMagick/discussions/6724
-spec im_mime(binary(), mime_type()|undefined) -> mime_type().
im_mime(<<"PBM">>, MimeFile) when MimeFile =/= undefined -> MimeFile;
im_mime(<<"AI">>, <<"application/pdf">>) -> <<"application/pdf">>;
im_mime(Type, _) -> mime(Type).

%% @doc Map the type returned by ImageMagick to a mime type
%% @todo Add more imagemagick types, check the mime types
-spec mime( binary() ) -> mime_type().
mime(<<"JPEG">>)  -> <<"image/jpeg">>;
mime(<<"GIF">>)   -> <<"image/gif">>;
mime(<<"TIFF">>)  -> <<"image/tiff">>;
mime(<<"BMP">>)   -> <<"image/bmp">>;
mime(<<"PDF">>)   -> <<"application/pdf">>;
mime(<<"PS">>)    -> <<"application/postscript">>;
mime(<<"PS2">>)   -> <<"application/postscript">>;
mime(<<"PS3">>)   -> <<"application/postscript">>;
mime(<<"PNG">>)   -> <<"image/png">>;
mime(<<"PNG8">>)  -> <<"image/png">>;
mime(<<"PNG24">>) -> <<"image/png">>;
mime(<<"PNG32">>) -> <<"image/png">>;
mime(<<"SVG">>)   -> <<"image/svg+xml">>;
mime(Type)        -> <<"image/", (z_string:to_lower(Type))/binary>>.


% Some PDFs start with a '\t' or whitespace, which causes 'file'
% to mis-guess the filetype.
-spec identify_magicnumber( file:filename_all() ) -> {ok, mime_type()} | {error, term()}.
identify_magicnumber(File) ->
    case file:open(File, [read, raw, binary]) of
        {ok, Fd} ->
            R = file:read(Fd, 10),
            ok = file:close(Fd),
            case R of
                {ok, <<"%PDF", _/binary>>} ->
                    {ok, <<"application/pdf">>};
                {ok, <<Space, "%PDF", _/binary>>} when Space =:= $\t; Space =:= 32 ->
                    {ok, <<_, Data/binary>>} = file:read_file(File),
                    ok = file:write_file(File, Data),
                    {ok, <<"application/pdf">>};
                _ ->
                    {error, unknown}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Return the extension for a known mime type (eg. ".mov").
-spec extension(Mime) -> filename_extension()
    when Mime :: string()
               | binary()
               | {binary(), binary(), list()}.
extension(Mime) ->
    extension(Mime, undefined).

%% @doc Return the extension for a known mime type (eg. ".mov"). When
%% multiple extensions are found for the given mime type, returns the
%% one that is given as the preferred extension. Otherwise, it returns
%% the first extension.
-spec extension(Mime, PreferExtension, z:context()) -> filename_extension()
    when Mime :: string()
               | binary()
               | {binary(), binary(), list()},
         PreferExtension :: string()
                          | binary()
                          | undefined.
extension(Mime, PreferExtension, Context) when is_binary(Mime) ->
    case z_notifier:first(
                #media_identify_extension{
                    mime = maybe_binary(Mime),
                    preferred = maybe_binary(PreferExtension)},
                Context)
    of
        undefined ->
            extension(Mime, PreferExtension);
        Extension ->
            Extension
    end;
extension(Mime, PreferExtension, Context) ->
    extension(z_convert:to_binary(Mime), PreferExtension, Context).

maybe_binary(undefined) -> undefined;
maybe_binary({A, B, _}) -> <<A/binary, $/, B/binary>>;
maybe_binary(L) -> z_convert:to_binary(L).

-spec extension(Mime, PreferExtension) -> filename_extension()
    when Mime :: string()
               | binary()
               | {binary(), binary(), list()},
         PreferExtension :: string()
                          | binary()
                          | undefined.
extension({A, B, _}, PreferExtension) ->
    extension(<<A/binary, $/, B/binary>>, PreferExtension);
extension(Mime, PreferExtension) when is_list(Mime) ->
    extension(list_to_binary(Mime), PreferExtension);
extension(<<"application/ld+json">>, _PreferExtension) -> <<".jsonld">>;
extension(<<"image/jpeg">>, _PreferExtension) -> <<".jpg">>;
extension(<<"application/vnd.ms-excel">>, _) -> <<".xls">>;
extension(<<"text/plain">>, _PreferExtension) -> <<".txt">>;
extension(<<"audio/wav">>, _PreferExtension) -> <<".wav">>;
extension(<<"audio/x-m4a">>, _PreferExtension) -> <<".m4a">>;
extension(<<"audio/mp4">>, _PreferExtension) -> <<".m4a">>;
extension(<<"audio/mp4a-latm">>, _PreferExtension) -> <<".m4a">>;
extension(<<"application/pgp-keys">>, _PreferExtension) -> <<".asc">>;
extension(<<"application/x-bert">>, _PreferExtension) -> <<".bert">>;
% Fonts have since 2017 their own mime types- https://tools.ietf.org/html/rfc8081#section-4.4.5
extension(<<"font/woff">>, _PreferExtension) -> <<".woff">>;
extension(<<"font/woff2">>, _PreferExtension) -> <<".woff2">>;
extension(<<"font/ttf">>, _PreferExtension) -> <<".ttf">>;
extension(<<"font/eot">>, _PreferExtension) -> <<".eot">>;
extension(<<"font/otf">>, _PreferExtension) -> <<".otf">>;
extension(<<"image/heic">>, _PreferExtension) -> <<".heic">>;
extension(<<"message/rfc822">>, _PreferExtension) -> <<".eml">>;
extension(Mime, undefined) ->
    Extensions = mimetypes:extensions(Mime),
    first_extension(Extensions);
extension(Mime, PreferExtension) ->
    %% convert prefer extension to something that mimetypes likes
    Ext1 = z_string:to_lower(PreferExtension),
    Ext2 = case Ext1 of
               <<$.,Rest/binary>> -> Rest;
               _ -> Ext1
           end,
    Extensions = mimetypes:extensions(Mime),
    case lists:member(Ext2, Extensions) of
        true -> <<$., Ext2/binary>>;
        false -> first_extension(Extensions)
    end.

first_extension([]) ->
    <<".bin">>;
first_extension([ Ext | _ ]) ->
    <<$., Ext/binary>>.


%% @doc  Guess the mime type of a file by the extension of its filename.
-spec guess_mime( file:filename_all() ) -> mime_type().
guess_mime(File) ->
    case z_string:to_lower( filename:extension( File ) ) of
        <<".jsonld">> -> <<"application/ld+json">>;
        <<".bert">> -> <<"application/x-bert">>;
        % Fonts have since 2017 their own mime types- https://tools.ietf.org/html/rfc8081#section-4.4.5
        <<".woff">> -> <<"font/woff">>;
        <<".woff2">> -> <<"font/woff2">>;
        <<".ttf">> -> <<"font/ttf">>;
        <<".eot">> -> <<"font/eot">>;
        <<".otf">> -> <<"font/otf">>;
        <<".heic">> -> <<"image/heic">>;
        <<".mjs">> -> <<"text/javascript">>;
        <<".eml">> -> <<"message/rfc822">>;
        <<".", Ext/binary>> ->
            [Mime|_] = mimetypes:ext_to_mimes(Ext),
            maybe_map_mime(Mime);
        _ ->
            <<"application/octet-stream">>
    end.

maybe_map_mime(<<"audio/x-wav">>) -> <<"audio/wav">>;
maybe_map_mime(<<"audio/mp4a-latm">>) -> <<"audio/mp4">>;
maybe_map_mime(Mime) -> Mime.

% Fetch the EXIF information from the file, we remove the maker_note as it can be huge
-spec exif( file:filename_all() ) -> map().
exif(File) ->
    try
        case erlang_exif:read(File, maps) of
            {ok, ExifMap} ->
                maps:fold(
                    fun
                        (maker_note, _Val, Acc) ->
                            Acc;
                        (user_comment, _Val, Acc) ->
                            Acc;
                        (Tag, Val, Acc) ->
                            case is_zero_val(Val) of
                                true ->
                                    Acc;
                                false ->
                                    Acc#{ atom_to_binary(Tag, utf8) => Val }
                            end
                    end,
                    #{},
                    ExifMap);
            {error, _} ->
                #{}
        end
    catch
        A:B:Stacktrace ->
            ?LOG_ERROR(#{
                text => <<"Error reading exif data from file">>,
                in => zotonic_core,
                result => A,
                reason => B,
                filename => File,
                stack => Stacktrace
            }),
            #{}
    end.


is_zero_val(L) when is_list(L) ->
    lists:all(fun(C) -> C =:= 0 end, L);
is_zero_val(L) when is_binary(L) ->
    lists:all(fun(C) -> C =:= 0 end, z_convert:to_list(L));
is_zero_val(_) ->
    false.

%% Detect the exif rotation in an image and swaps width/height accordingly.
-spec exif_orientation( map() ) -> 1|2|3|4|5|6|7|8.
exif_orientation(#{ <<"orientation">> := <<"Top-left">> }) -> 1;
exif_orientation(#{ <<"orientation">> := <<"Top-right">> }) -> 2;
exif_orientation(#{ <<"orientation">> := <<"Bottom-right">> }) -> 3;
exif_orientation(#{ <<"orientation">> := <<"Bottom-left">> }) -> 4;
exif_orientation(#{ <<"orientation">> := <<"Left-top">> }) -> 5;
exif_orientation(#{ <<"orientation">> := <<"Right-top">> }) -> 6;
exif_orientation(#{ <<"orientation">> := <<"Right-bottom">> }) -> 7;
exif_orientation(#{ <<"orientation">> := <<"Left-bottom">> }) -> 8;
exif_orientation(_) -> 1.


%% See also http://www.awaresystems.be/imaging/tiff/tifftags/privateifd/exif/subjectarea.html
exif_subject_point(Exif, Orientation, Width, Height) ->
    Point = extract_subject_point(Exif),
    Point1 = maybe_resize_point(Point, Exif, Width, Height),
    maybe_rotate(Orientation, Point1, Width, Height).

extract_subject_point(#{ <<"subject_area">> := [ X, Y ] }) -> {X, Y};
extract_subject_point(#{ <<"subject_area">> := [X, Y, _Radius] }) -> {X, Y};
extract_subject_point(#{ <<"subject_area">> := [X, Y, _W, _H] }) -> {X, Y};
extract_subject_point(_) -> undefined.

maybe_resize_point(undefined, _Exif, _Width, _Height) ->
    undefined;
maybe_resize_point({X, Y}, Exif, Width, Height) ->
    ExifWidth = maps:get(<<"pixel_x_dimension">>, Exif, undefined),
    ExifHeight = maps:get(<<"pixel_y_dimension">>, Exif, undefined),
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
    ExifWidth = maps:get(<<"pixel_x_dimension">>, Exif, 0),
    ExifHeight = maps:get(<<"pixel_y_dimension">>, Exif, 0),
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
-spec is_mime_compressed(binary()) -> boolean().
is_mime_compressed(<<"text/", _/binary>>)                        -> false;
is_mime_compressed(<<"image/svgz", _/binary>>)                   -> true;
is_mime_compressed(<<"image/svg", _/binary>>)                    -> false;
is_mime_compressed(<<"image/", _/binary>>)                       -> true;
is_mime_compressed(<<"video/", _/binary>>)                       -> true;
is_mime_compressed(<<"audio/x-wav">>)                            -> false;
is_mime_compressed(<<"audio/wav">>)                              -> false;
is_mime_compressed(<<"audio/", _/binary>>)                       -> true;
is_mime_compressed(<<"application/x-compres", _/binary>>)        -> true;
is_mime_compressed(<<"application/zip">>)                        -> true;
is_mime_compressed(<<"application/x-gz", _/binary>>)             -> true;
is_mime_compressed(<<"application/x-rar">>)                      -> true;
is_mime_compressed(<<"application/x-bzip2">>)                    -> true;
is_mime_compressed(<<"application/x-font-woff">>)                -> true;
is_mime_compressed(<<"application/vnd.oasis.opendocument.", _/binary>>) -> true;
is_mime_compressed(<<"application/vnd.openxml", _/binary>>)      -> true;
is_mime_compressed(<<"application/x-shockwave-flash">>)          -> true;
is_mime_compressed(_)                                            -> false.


