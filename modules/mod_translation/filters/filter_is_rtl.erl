%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Check if the given language is a rtl or ltr language

%% Copyright 2011 Marc Worrell
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

-module(filter_is_rtl).
-export([
    is_rtl/2
]).

-include("zotonic.hrl").

is_rtl(Id, Context) when is_integer(Id) ->
    is_rtl(filter_language:language(Id, Context), Context);
is_rtl([Lang|_] = Langs, Context) when is_atom(Lang) ->
    is_rtl(filter_language:language(Langs, Context), Context);
is_rtl(ar, _Context) -> true;
is_rtl(fa, _Context) -> true;
is_rtl(he, _Context) -> true;
is_rtl("ar", _Context) -> true;
is_rtl("fa", _Context) -> true;
is_rtl("he", _Context) -> true;
is_rtl(<<"ar">>, _Context) -> true;
is_rtl(<<"fa">>, _Context) -> true;
is_rtl(<<"he">>, _Context) -> true;
is_rtl(_, _Context) -> false.
