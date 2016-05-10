%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%%
%% @doc Check if an identified file is acceptable as upload.

%% Copyright 2016 Marc Worrell
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

-module(z_media_sanitize).

-export([
    is_file_acceptable/2
    ]).

% For testing
-export([
    is_acceptable_svg/1
    ]).

%% @doc Check the contents of an identified file, to see if it is acceptable for further processing.
%%      Catches files that might be problematic for ImageMagick or other file processors.
is_file_acceptable(File, MediaProps) when is_list(MediaProps) ->
    Mime = z_convert:to_binary(proplists:get_value(mime, MediaProps)),
    is_file_acceptable_1(Mime, File, MediaProps).

is_file_acceptable_1(<<"image/svg+xml">>, File, _MediaProps) ->
    {ok, Bin} = file:read_file(File),
    is_acceptable_svg(Bin);
is_file_acceptable_1(_Mime, _File, _MediaProps) ->
    true.

%% @doc Don't accept SVG files referring to an external resource. 
%%      Refuses anything containing "href=" and "url(...)" not referring to local ids.
is_acceptable_svg(Bin) ->
    Bin1 = re:replace(Bin, <<"href=\"#">>, <<>>),
    Bin2 = re:replace(Bin1, <<"url\\(#">>, <<>>),
    case re:run(Bin2, "([^a-zA-Z][hH][rR][eE][fF]\\s*=|[uU][rR][lL]\\s*\\()") of
        {match, _} -> false;
        nomatch -> true
    end.

