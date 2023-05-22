%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell
%% @doc 'url_abs' filter, generates an url with hostname/protocol.

%% Copyright 2020 Marc Worrell
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

-module(filter_url_abs).
-export([url_abs/2]).

url_abs(undefined, _Context) ->
    undefined;
url_abs(<<>>, Context) ->
    z_context:abs_url(<<>>, Context);
url_abs(<<$/, _/binary>> = Url, Context) ->
    z_context:abs_url(Url, Context);
url_abs(<<"http:", _/binary>> = Url, _Context) ->
    Url;
url_abs(<<"https:", _/binary>> = Url, _Context) ->
    Url;
url_abs(Name, Context) ->
    case filter_url:url(Name, Context) of
        undefined -> undefined;
        Url -> z_context:abs_url(Url, Context)
    end.
