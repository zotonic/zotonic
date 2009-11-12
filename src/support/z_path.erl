%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-08-05
%% @doc Defines all paths for files and directories of a site.

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

-module(z_path).
-author("Marc Worrell <marc@worrell.nl").

-export([
    media_preview/1,
    media_archive/1
]).

-include("zotonic.hrl").

%% @doc Return the path to the media preview directory
%% @spec media_preview(#context) -> filename()
media_preview(#context{host=Host}) ->
    filename:join([code:lib_dir(zotonic, priv), "sites", Host, "files", "preview"]).

%% @doc Return the path to the media archive directory
%% @spec media_archive(#context) -> filename()
media_archive(#context{host=Host}) ->
    filename:join([code:lib_dir(zotonic, priv), "sites", Host, "files", "archive"]).
