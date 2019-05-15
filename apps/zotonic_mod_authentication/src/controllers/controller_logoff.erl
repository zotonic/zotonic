%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2019 Marc Worrell
%% @doc Log off an user, remove "autologon" cookies

%% Copyright 2010-2019 Marc Worrell
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
    resource_exists/1,
    previously_existed/1,
    moved_temporarily/1
]).

resource_exists(Context) ->
    {false, z_authentication_tokens:reset_cookies(Context)}.

previously_existed(Context) ->
    {true, Context}.

moved_temporarily(Context) ->
    Location = z_context:get_q(<<"p">>, Context, <<"/">>),
    {{true, Location}, Context}.
