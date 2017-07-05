%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2015 Marc Worrell
%% @doc Convert a value to integer

%% Copyright 2010-2015 Marc Worrell
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

-module(filter_to_integer).
-export([to_integer/2]).

to_integer(undefined, _Context) ->
    undefined;
to_integer({trans, _} = Tr, Context) ->
    to_integer(z_trans:lookup_fallback(Tr, Context), Context);
to_integer(N, _Context) ->
	try
	    z_convert:to_integer(N)
	catch
		_:_ -> undefined
	end.

