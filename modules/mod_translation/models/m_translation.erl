%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell
%%
%% @doc Model for access to language lists etc

%% Copyright 2013 Marc Worrell
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

-module(m_translation).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    language_list/1,
    language_list_enabled/1
]).

-include("zotonic.hrl").

m_find_value(language, #m{value=undefined}, Context) ->
	z_context:language(Context);
m_find_value(default_language, #m{value=undefined}, Context) ->
	z_trans:default_language(Context);
m_find_value(language_list, #m{value=undefined}, Context) ->
	language_list(Context);
m_find_value(language_list_enabled, #m{value=undefined}, Context) ->
	language_list_enabled(Context).

m_to_list(#m{}, _Context) ->
	[].

m_value(#m{}, _Context) ->
	undefined.


language_list(Context) ->
	case m_config:get(i18n, language_list, Context) of
		undefined -> 
			[];
		Config ->
			case proplists:get_value(list, Config, Context) of
				L when is_list(L) -> L;
				_ -> []
			end
	end.

language_list_enabled(Context) ->
	lists:filter(fun({_Code,Props}) ->
					proplists:get_value(is_enabled, Props)
				 end,
				 language_list(Context)).
