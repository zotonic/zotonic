%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2017 Marc Worrell
%% @doc Test service for OAuth requests and app/token registry.

%% Copyright 2017 Marc Worrell
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

-module(service_oauth_test).

-author("Marc Worrell <marc@worrell.nl>").

-svc_title("Test service for OAuth.").
-svc_needauth(true).

-export([
    process_get/1,
    process_post/1
]).

process_post(Context) ->
    check_auth(Context).

process_get(Context) ->
    check_auth(Context).

check_auth(Context) ->
    case z_acl:user(Context) of
        undefined -> #{<<"user">> => <<"anon">>};
        _UserId -> #{<<"user">> => <<"auth">>}
    end.

