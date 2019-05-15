%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2017 Marc Worrell
%% @doc Check if an entered username is unique

%% Copyright 2010-2017 Marc Worrell
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

-module(validator_admin_identity_username_unique).
-include_lib("zotonic_core/include/zotonic.hrl").
-export([
    render_validator/5,
    validate/5,
    event/2
]).

render_validator(username_unique, TriggerId, TargetId, Args, Context)  ->
    {_PostbackJS, PostbackInfo} = z_render:make_postback({validate, Args}, 'postback', TriggerId, TargetId, ?MODULE, Context),
    JsObject = z_utils:js_object(z_validation:rename_args([{z_postback, PostbackInfo}|Args])),
    Script = [<<"z_add_validator(\"">>,TriggerId,<<"\", \"postback\", ">>, JsObject, <<");\n">>],
    {Args, Script}.

%% @spec validate(Type, TriggerId, Value, Args, Context) -> {{ok,AcceptedValue}, NewContext} | {{error,Id,Error}, NewContext}
%%          Error = invalid | novalue | {script, Script} | novalidator | string()
validate(username_unique, Id, Value, Args, Context) ->
    UserId = z_convert:to_integer(proplists:get_value(id, Args)),
    case z_string:trim(Value) of
        [] -> {{ok, <<>>}, Context};
        <<>> -> {{ok, <<>>}, Context};
        Username ->
            case m_identity:is_reserved_name(Username) of
                true ->
                    {{error, Id, ?__("Sorry, this username is already in use. Please try another one.", Context)}, Context};
                false ->
                    case m_identity:lookup_by_username(Username, Context) of
                        undefined ->
                            {{ok, Username}, Context};
                        Identity ->
                            case proplists:get_value(rsc_id, Identity) of
                                UserId -> {{ok, Username}, Context};
                                _Other -> {{error, Id, ?__("Sorry, this username is already in use. Please try another one.", Context)}, Context}
                            end
                    end
            end
    end.

%% @spec event(Event, Context) -> Context
%% @doc Handle the validation during form entry. Hides/shows the message element with id: TriggerId + "_username_unique_error" (if present).
event(#postback{message={validate, Args}, trigger=TriggerId}, Context) ->
    Value = z_context:get_q(triggervalue, Context),
    {IsValid, ContextValidated} = case validate(username_unique, TriggerId, Value, Args, Context) of
        {{ok, _}, ContextOk} ->
            {<<"true">>, z_render:wire({fade_out, [{target, <<TriggerId/binary, "_username_unique_error">>}]}, ContextOk)};
        {{error, Id, _} = Error, ContextScript} ->
            {<<"false">>, z_render:wire({fade_in, [{target, <<TriggerId/binary, "_username_unique_error">>}]},
            z_validation:report_errors([{Id,Error}], ContextScript))}
    end,
    z_render:add_script(["z_async_validation_result('",TriggerId,"', ",IsValid,", '",z_utils:js_escape(Value),"');"], ContextValidated).
