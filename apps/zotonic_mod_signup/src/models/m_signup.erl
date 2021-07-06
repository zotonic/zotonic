%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Model to support signup views.

%% Copyright 2021 Marc Worrell
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

-module(m_signup).

-behaviour(zotonic_model).

-export([
    m_get/3,
    confirm_redirect/1
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"confirm_redirect">> | Rest ], _Msg, Context) ->
    Url = confirm_redirect(Context),
    {ok, {Url, Rest}}.


-spec confirm_redirect( z:context() ) -> binary().
confirm_redirect(Context) ->
    Url = case z_acl:user(Context) of
        undefined ->
            z_dispatcher:url_for(home, Context);
        UserId ->
            case z_notifier:first(#signup_confirm_redirect{id=UserId}, Context) of
                undefined -> m_rsc:p(UserId, page_url, Context);
                Loc -> Loc
            end
    end,
    z_convert:to_binary(Url).
