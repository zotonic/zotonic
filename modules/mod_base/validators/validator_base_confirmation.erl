%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Validator for checking if the input is the same as another input.

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

-module(validator_base_confirmation).
-include("zotonic.hrl").
-export([render_validator/5, validate/5]).

render_validator(confirmation, TriggerId, _TargetId, Args, Context)  ->
    MatchField = proplists:get_value(match, Args),
	JsObject   = z_utils:js_object(z_validation:rename_args(Args)), 
	Script     = [<<"z_add_validator(\"">>,TriggerId,<<"\", \"confirmation\", ">>, JsObject, <<");\n">>],
	{[MatchField], Script, Context}.


%% @spec validate(Type, TriggerId, Values, Args, Context) -> {ok,AcceptedValue} | {error,Id,Error}
%%          Error = invalid | novalue | {script, Script}
validate(confirmation, Id, Value, [MatchField], Context) ->
    MatchValue = z_validation:get_q(MatchField, Context),
    if 
        MatchValue =:= Value -> {{ok, Value}, Context};
        true -> {{error, Id, invalid}, Context}
    end.
