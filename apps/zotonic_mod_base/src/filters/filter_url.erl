%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell
%% @doc 'url' filter, generates an url

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

-module(filter_url).
-export([url/2]).

url(undefined, _Context) ->
    undefined;
url(Name, Context) when is_atom(Name) ->
    z_dispatcher:url_for(Name, Context);
url({Name, Args}, Context) when is_atom(Name), is_list(Args) ->
    z_dispatcher:url_for(Name, Args, Context);
url(Name, Context) when is_binary(Name)->
    try binary_to_existing_atom(Name, utf8) of
        Atom -> url(Atom, Context)
    catch
        error:badarg -> undefined
    end;
url(Name, Context) when is_list(Name)->
    try list_to_existing_atom(Name) of
        Atom -> url(Atom, Context)
    catch
        error:badarg -> undefined
    end;
url(_Name, _Context) ->
    undefined.

