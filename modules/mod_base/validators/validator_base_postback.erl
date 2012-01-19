%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc Postback validator, checks using a server callback against an Erlang function.  

%% Copyright 2010 Marc Worrell
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

-module(validator_base_postback).
-include("zotonic.hrl").
-export([
    render_validator/5,
    validate/5,
    event/2
]).

render_validator(postback, TriggerId, TargetId, Args, Context)  ->
	{_PostbackJS, PostbackInfo} = z_render:make_postback({validate, Args}, 'postback', TriggerId, TargetId, ?MODULE, Context),
	JsObject = z_utils:js_object(z_validation:rename_args([{z_postback, PostbackInfo}|Args])),
	Script = [<<"z_add_validator(\"">>,TriggerId,<<"\", \"postback\", ">>, JsObject, <<");\n">>],
	{Args, Script, Context}.

%% @spec validate(Type, TriggerId, Value, Args, Context) -> {{ok,AcceptedValue}, NewContext} | {{error,Id,Error}, NewContext}
%%          Error = invalid | novalue | {script, Script} | novalidator | string()
validate(postback, Id, Value, Args, Context) ->
    case proplists:get_value(delegate, Args) of
        undefined ->
            case proplists:get_value(event, Args) of
                undefined ->
                    %% No validator defined, default to being safe
                    {{error, Id, novalidator}, Context};
                Event ->
                    case z_notifier:first({z_convert:to_atom(Event), {postback, Id, Value, Args}}, Context) of
                        undefined -> {{error, Id, invalid}, Context};
                        Result -> Result
                    end
            end;
        Delegate ->
            Delegate:validate(postback, Id, Value, Args, Context)
    end.


%% @spec event(Event, Context) -> Context
%% @doc Handle the validation during form entry.
event(#postback{message={validate, Args}, trigger=TriggerId}, Context) ->
    Value = z_context:get_q(triggervalue, Context),
    IsValid = case validate(postback, TriggerId, Value, Args, Context) of
        {{ok, _},ContextValidated} -> 
            "true";
        {{error, Id, _} = Error, ContextScript} -> 
            ContextValidated = z_validation:report_errors([{Id,Error}], ContextScript),
            "false"
    end,
    z_script:add_script(["z_async_validation_result('",TriggerId,"', ",IsValid,", '",z_utils:js_escape(Value),"');"], ContextValidated).
