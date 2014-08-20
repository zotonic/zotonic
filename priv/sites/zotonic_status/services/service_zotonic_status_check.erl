%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2009-10-03
%% @doc Retrieve a full dump of an object.

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

-module(service_zotonic_status_check).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-svc_title("Checks if all sites are running.").
-svc_needauth(false).

-export([process_get/2]).

-include_lib("zotonic.hrl").


process_get(_ReqData, _Context) ->
    Statuses = [Stat || [_Site, Stat |_] <- z_sites_manager:get_sites_status()],
    Result = case lists:member(failed, Statuses) orelse
                 lists:member(retrying, Statuses) of
                 true ->
                     fail;
                 false ->
                     ok
             end,
    {struct, [{status, Result}]}.

