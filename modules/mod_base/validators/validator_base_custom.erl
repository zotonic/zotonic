%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Custom javascript based validators

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

-module(validator_base_custom).
-include("zotonic.hrl").
-export([render_validator/5, validate/5]).

render_validator(custom, TriggerId, _TargetId, Args, Context)  ->
    JsObject   = z_utils:js_object(z_validation:rename_args(Args)), 
    Script     = [<<"z_add_validator(\"">>,TriggerId,<<"\", \"custom\", ">>, JsObject, <<");\n">>],
    {[], Script, Context}.

%% @spec validate(Type, Name, Values, Args, Context) -> {ok, AcceptedValue} | {error,Id,Error}
%%          Error = invalid | novalue
validate(custom, _Id, Value, _Args, Context) ->
    {{ok, Value}, Context}.
