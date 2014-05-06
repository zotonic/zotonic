%% @author Mawuli Adzaku <mawuli@mawuli.me>
%% @copyright 2014 Mawuli Adzaku
%% @doc 'md5' filter, translate a string to an md5 hex value

%% Copyright 2014 Mawuli Adzaku
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

-module(filter_md5).
-export([md5/2]).


md5(undefined, _Context) ->
    undefined;
md5(Input, _Context) ->
    erlang:iolist_to_binary(z_utils:hex_encode(crypto:hash(md5, z_convert:to_binary(Input)))).
