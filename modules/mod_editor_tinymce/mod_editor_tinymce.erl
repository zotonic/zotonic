%% @author Arthur Clemens <arthurclemens@gmail.com>
%% @copyright 2014 Arthur Clemens
%% Date: 2014-05-11
%% @doc TinyMCE Editor module

%% Copyright 2014 Arthur Clemens
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
-export([init/1]).

init(Context) ->
    % set to default version 4.0.26
    case m_config:get(?MODULE, version, Context) of 
        undefined -> m_config:set_value(?MODULE, version, "4.0.26", Context);
        _ -> undefined
    end,
	ok.

