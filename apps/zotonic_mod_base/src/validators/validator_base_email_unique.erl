%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2026 Marc Worrell
%% @doc Verify if an email address is already couple to another account.
%% @end

%% Copyright 2013-2026 Maximonster Interactive Things BV
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

-module(validator_base_email_unique).
-include_lib("zotonic_core/include/zotonic.hrl").
-export([
    render_validator/5,
    validate/5,
    event/2,
    is_validated_account/2
]).

render_validator(email_unique, TriggerId, TargetId, Args, Context)  ->
    {_PostbackJS, PostbackInfo} = z_render:make_postback({validate, Args}, 'postback', TriggerId, TargetId, ?MODULE, Context),
    JsObject = z_utils:js_object(z_validation:rename_args([{z_postback, PostbackInfo}|Args])),
    Script = [<<"z_add_validator(\"">>,TriggerId,<<"\", \"postback\", ">>, JsObject, <<");\n">>],
    {Args, Script}.

validate(email_unique, _TriggerId, Value, _Args, Context) ->
    Email = z_string:trim(Value),
    {{ok, Email}, Context}.

%% @doc Handle the validation during form entry.
event(#postback{message={validate, _Args}, trigger=TriggerId}, Context) ->
    EmailValue = z_convert:to_binary(z_context:get_q(<<"triggervalue">>, Context)),
    EmailTrimmed = z_string:trim(EmailValue),
    IsValidatedAccount = is_validated_account(EmailTrimmed, Context),
    TriggerIdS = z_convert:to_list(TriggerId),
    Context1 = case IsValidatedAccount of
        true -> z_render:wire({fade_in, [{target, TriggerIdS ++ "_email_unique_error"}]}, Context);
        false -> z_render:wire({fade_out, [{target, TriggerIdS ++ "_email_unique_error"}]}, Context)
    end,
    z_render:add_script([
        "z_async_validation_result('",
                TriggerId, "',",
                z_convert:to_binary(not IsValidatedAccount), $,,
                "'", z_utils:js_escape(EmailValue),"');"
            ], Context1).

is_validated_account("", _Context) ->
    false;
is_validated_account(<<>>, _Context) ->
    false;
is_validated_account(Email, Context) ->
    Idns = m_identity:lookup_by_type_and_key_multi(email, Email, Context),
    lists:any(
        fun(Idn) ->
            RscId = proplists:get_value(rsc_id, Idn),
            m_identity:is_user(RscId, Context)
       end,
       Idns).
