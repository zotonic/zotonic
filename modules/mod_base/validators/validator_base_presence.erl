%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Validator for checking if a input has been filled/checked in.

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

-module(validator_base_presence).
-include("zotonic.hrl").
-export([render_validator/5, validate/5]).

render_validator(presence, TriggerId, _TargetId, Args, Context)  ->
	JsObject = z_utils:js_object(z_validation:rename_args(Args)), 
	Script   = [<<"z_add_validator(\"">>,TriggerId,<<"\", \"presence\", ">>, JsObject, <<");\n">>],
	{[], Script, Context}.


%% @spec validate(Type, TriggerId, Values, Args, Context) -> {ok,AcceptedValue} | {error,Id,Error}
%%          Error = invalid | novalue | {script, Script}
validate(presence, Id, undefined, _Args, Context) -> 
    {{error, Id, novalue}, Context};
validate(presence, Id, [], _Args, Context) ->
    {{error, Id, novalue}, Context};
validate(presence, _Id, #upload{} = Value, _Args, Context) ->
    {{ok, Value}, Context};
validate(presence, Id, Value,     _Args, Context) ->
    case z_string:trim(Value) of
        [] -> {{error, Id, invalid}, Context};
        _Trimmed -> {{ok, Value}, Context}
    end.
