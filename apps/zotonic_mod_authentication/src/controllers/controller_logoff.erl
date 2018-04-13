%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-05-08
%% @doc Log off an user, remove "rememberme" cookies

%% Copyright 2010 Marc Worrell
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
    charsets_provided/1,
    content_types_provided/1,
    resource_exists/1,
    previously_existed/1,
    moved_temporarily/1,
    provide_content/1
]).
-export([reset_rememberme_cookie_and_logoff/1]).

charsets_provided(Context) ->
    {[<<"utf-8">>], Context}.

content_types_provided(Context) ->
    {[{<<"text/html">>, provide_content}], Context}.

resource_exists(Context) ->
    % TODO: when there is javascript in the context, then return the javascript
    % to be executed, together with a
    % redirect action.  This is the case when we have a Facebook connect log off.
    Context2 = z_context:ensure_qs(Context),
    Context3 = reset_rememberme_cookie_and_logoff(Context2),
    case lists:flatten(z_script:get_script(Context3)) of
        [] -> {false, Context3};
        _Script -> {true, Context3}
    end.

previously_existed(Context) ->
    {true, Context}.

moved_temporarily(Context) ->
    Location = z_context:get_q(<<"p">>, Context, <<"/">>),
    {{true, Location}, Context}.

provide_content(Context) ->
    Context1 = z_context:set_resp_header(<<"x-robots-tag">>, <<"noindex">>, Context),
    Rendered = z_template:render("logoff.tpl", z_context:get_all(Context1), Context1),
    z_context:output(Rendered, Context1).

% @doc Logoff and reset the rememberme cookie.
reset_rememberme_cookie_and_logoff(Context) ->
    ContextNoCookie = controller_logon:reset_rememberme_cookie(Context),
    ContextLogOff = z_auth:logoff(ContextNoCookie),
    {ok, ContextNoSession} = z_session_manager:stop_session(ContextLogOff),
    ContextNoSession.
