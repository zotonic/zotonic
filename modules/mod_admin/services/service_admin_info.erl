%% @author Arthur Clemens <arthurclemens@gmail.com>
%% @copyright 2014 Arthur Clemens
%% Date: 2014-05-09
%% @doc Get information about the admin module. Currently only pivot queue count.

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

-module(service_admin_info).
-author("Arthur Clemens <arthurclemens@gmail.com>").

-svc_title("Basic information about the admin module.").
-svc_needauth(true).

-export([process_get/2]).

-include_lib("zotonic.hrl").

process_get(_ReqData, Context) ->
    {struct, [
        {pivot_queue_count, z_db:q1("SELECT COUNT(*) FROM rsc_pivot_queue", Context)}
    ]}.