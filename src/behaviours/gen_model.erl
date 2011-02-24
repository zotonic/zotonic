%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-12
%%
%% @doc Model behaviour

%% Copyright 2009 Marc Worrell
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

-module(gen_model).
-author("Marc Worrell <marc@worrell.nl").

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
        {m_find_value, 3},
        {m_value, 2},
        {m_to_list, 2}
     ];
behaviour_info(_Other) ->
    undefined.
