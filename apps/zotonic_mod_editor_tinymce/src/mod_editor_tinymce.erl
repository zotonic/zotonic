%% @author Arthur Clemens <arthurclemens@gmail.com>
%% @copyright 2014-2025 Arthur Clemens
%% @doc TinyMCE Editor module
%% @end

%% Copyright 2014-2025 Arthur Clemens
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

-module(mod_editor_tinymce).
-author("Arthur Clemens <arthurclemens@gmail.com>").

-mod_title("TinyMCE Editor").
-mod_description("Rich Text Editor using TinyMCE.").
-mod_prio(500).
-mod_config([
        #{
            name => version,
            type => string,
            default => "newest",
            description => "The TinyMCE version to use. Either the version number or 'newest'."
        }
    ]).

-export([
    event/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

event(#z_msg_v1{data=Data}, Context) ->
    handle_cmd(proplists:get_value(<<"cmd">>, Data), Data, Context).

handle_cmd(<<"zmedia-props">>, Data, Context) ->
    {<<"options">>, Options} = proplists:lookup(<<"options">>, Data),
    {<<"id">>, Id} = proplists:lookup(<<"id">>, Data),
    z_render:dialog(
        ?__("Media Properties", Context),
        "_tinymce_dialog_zmedia_props.tpl",
        [
            {id, m_rsc:rid(Id, Context)},
            {options, Options},
            {level, 5}
        ],
        Context).
