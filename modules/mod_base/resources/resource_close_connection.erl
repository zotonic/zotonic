%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-05-13
%% @doc Simple resource that closes the connection, used when uploading forms with Ajax in Safari browsers.

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

-module(resource_close_connection).
-author("Marc Worrell <marc@worrell.nl").

-export([
    init/1,
    to_html/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

init(_Args) -> {ok, []}.

to_html(ReqData, State) ->
    ReqData2 = wrq:set_resp_header("Connection", "close", ReqData),
    {"", ReqData2, State}.
