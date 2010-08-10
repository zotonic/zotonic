%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-06-08
%% @doc The base module, implementing basic Zotonic scomps, actions, models and validators.

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

-module(mod_base).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Zotonic Base").
-mod_description("Base supplies all basic scomps, actions and validators.").
-mod_prio(9999).

-include_lib("zotonic.hrl").

%% interface functions
-export([
	observe_media_stillimage/2
]).

%% @doc Return the filename of a still image to be used for image tags.
%% @spec media_stillimage(Notification, _Context) -> undefined | {ok, Filename}
observe_media_stillimage({media_stillimage, _Id, Props}, Context) ->
    Mime = proplists:get_value(mime, Props),
	case z_media_preview:can_generate_preview(Mime) of
		true ->
			%% Let media preview handle this.
			undefined;
		false ->
			%% Serve an existing icon.
			[A,B] = string:tokens(z_convert:to_list(Mime), "/"),
			case z_module_indexer:find(lib, "images/mimeicons/"++A++[$-,B]++".png", Context) of
				{ok, File} -> {ok, File};
				{error, enoent} ->
					case z_module_indexer:find(lib, "images/mimeicons/"++A++".png", Context) of
						{ok, File} -> {ok, File};
						{error, enoent} -> {ok, "lib/images/mimeicons/application-octet-stream.png"}
					end
			end
    end.

%%====================================================================
%% support functions
%%====================================================================

