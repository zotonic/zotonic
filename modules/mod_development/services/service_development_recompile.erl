%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2009-10-06
%% @doc Remotely recompile and flush.

%% Copyright 2009 Arjan Scherpenisse
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

-module(service_development_recompile).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-svc_title("Remotely code reload / recompile and flush.").
-svc_needauth(false).

-export([process_get/2]).

-include_lib("zotonic.hrl").


process_get(_ReqData, _Context) ->
    try
        z:m()
    catch
        _:_ ->
            undefined
    end,
    "OK".
