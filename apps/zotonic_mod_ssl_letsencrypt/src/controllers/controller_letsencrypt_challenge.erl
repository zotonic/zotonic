%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Handle the challenge requested by Letsencrypt

%% Copyright 2016 Marc Worrell
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

-module(controller_letsencrypt_challenge).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    allowed_methods/1,
    content_types_provided/1,
    process/4
]).

allowed_methods(Context) ->
    {[ <<"GET">> ], Context}.

content_types_provided(Context) ->
    {[ {<<"text">>, <<"plain">>, []} ], Context}.

process(_Method, _AcceptedCT, _ProvidedCT, Context0) ->
    Context = z_context:set_noindex_header(z_context:set_nocache_headers(Context0)),
    Host = m_req:get(host, Context),
    {ok, Challenges} = mod_ssl_letsencrypt:get_challenge(Context),
    Token = z_context:get_q(<<"token">>, Context),
    case maps:get(Host, Challenges, undefined) of
        #{token := Token, thumbprint := Thumbprint} ->
            {Thumbprint, Context};
        _X ->
            {{halt, 404}, Context}
    end.
