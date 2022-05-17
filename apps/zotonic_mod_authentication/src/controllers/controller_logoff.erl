%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2021 Marc Worrell
%% @doc Log off an user, remove "autologon" cookies

%% Copyright 2010-2021 Marc Worrell
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

-module(controller_logoff).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    resource_exists/1,
    previously_existed/1,
    moved_temporarily/1
]).

service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

resource_exists(Context) ->
    Context1 = z_authentication_tokens:reset_cookies(Context),
    Context2 = z_auth:logoff(Context1),
    {false, Context2}.

previously_existed(Context) ->
    {true, Context}.

moved_temporarily(Context) ->
    Location = z_context:get_q(<<"p">>, Context, <<"/">>),
    Location1 = case z_context:is_site_url(Location, Context) of
        true -> Location;
        false -> <<"/">>
    end,
    LocationAbs = z_context:abs_url(Location1, Context),
    {{true, LocationAbs}, Context}.

