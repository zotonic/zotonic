%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2024 Marc Worrell
%% @doc Check if a password is acceptable for the site password restrictions.
%% @end

%% Copyright 2024 Marc Worrell
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

-module(validator_authentication_acceptable_password).

-export([
    render_validator/5,
    validate/5,
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

render_validator(acceptable_password, TriggerId, TargetId, Args, Context)  ->
    {_PostbackJS, PostbackInfo} = z_render:make_postback({validate, Args}, 'postback', TriggerId, TargetId, ?MODULE, Context),
    ValidatorArgs = z_utils:js_object(
        z_validation:rename_args([ {z_postback, PostbackInfo} | Args ])
    ),
    Script = [
        <<"z_add_validator(\"">>,TriggerId,<<"\", \"postback\", ">>, ValidatorArgs, <<");\n">>
    ],
    {Args, Script}.

validate(acceptable_password, _TriggerId, Password, _Args, Context) ->
    {{ok, Password}, Context}.

event(#postback{message={validate, _Args}, trigger=TriggerId}, Context) ->
    Password = z_convert:to_binary(z_context:get_q(<<"triggervalue">>, Context)),
    IsAcceptable = case m_authentication:acceptable_password(Password, Context) of
        ok -> true;
        {error, _} -> false
    end,
    z_render:add_script(iolist_to_binary([
        "z_async_validation_result(",
            "'", TriggerId, "',",
            z_convert:to_binary(IsAcceptable), ",",
            "'", z_utils:js_escape(Password), "'",
         ");"
    ]), Context).
