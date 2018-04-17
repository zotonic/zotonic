%% @doc Validator for checking name uniqueness

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
-module(validator_base_name_unique).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    render_validator/5,
    validate/5,
    event/2
]).

render_validator(name_unique, TriggerId, TargetId, Args, Context)  ->
    {_PostbackJS, PostbackInfo} = z_render:make_postback({validate, Args}, 'postback', TriggerId, TargetId, ?MODULE, Context),
    JsObject = z_utils:js_object(z_validation:rename_args([{z_postback, PostbackInfo} | Args])),
    Script = [<<"z_add_validator(\"">>, TriggerId, <<"\", \"postback\", ">>, JsObject, <<");\n">>],
    {Args, Script}.

-spec validate(name_unique, binary(), term(), list(), #context{}) ->
    {{ok, []}, #context{}} | {{error, m_rsc:resource(), atom() | binary()}, #context{}}.
validate(name_unique, Id, Value, Args, Context) ->
    Message = proplists:get_value(failure_message, Args, invalid),
    Value1 = z_string:trim(Value),
    case {z_utils:is_empty(Value1), z_string:to_name(Value1)} of
        {true, _} ->
            {{ok, <<>>}, Context};
        {false, <<>>} ->
            {{error, Id, Message}, Context};
        {false, <<"_">>} ->
            {{error, Id, Message}, Context};
        {false, Name} ->
            RscId = proplists:get_value(id, Args),
            case m_rsc:name_lookup(Name, Context) of
                undefined ->
                    {{ok, <<>>}, Context};
                RscId ->
                    {{ok, <<>>}, Context};
                _ ->
                    Message = proplists:get_value(failure_message, Args, invalid),
                    {{error, Id, Message}, Context}
            end
    end.

%% @doc Handle the validation during form entry.
-spec event(#postback{}, list()) -> #context{}.
event(#postback{message = {validate, Args}, trigger = TriggerId}, Context) ->
    Value = z_context:get_q(triggervalue, Context),
    {IsValid, ContextValidated} = case validate(name_unique, TriggerId, Value, Args, Context) of
        {{ok, _}, ContextOk} ->
            {"true", z_render:wire({fade_out, [{target, <<TriggerId/binary, "_name_unique_error">>}]}, ContextOk)};
        {{error, Id, _} = Error, ContextScript} ->
            {"false", z_render:wire({fade_in, [{target, <<TriggerId/binary, "_name_unique_error">>}]},
                z_validation:report_errors([{Id, Error}], ContextScript))}
    end,
    z_render:add_script(
        ["z_async_validation_result('", TriggerId, "', ", IsValid, ", '", z_utils:js_escape(Value), "');"],
        ContextValidated
    ).
