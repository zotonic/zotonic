%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2024 Marc Worrell
%% @doc Check if a resource has edges with a certain predicate. Useful when
%% editing a resource and the resource must have some edges, for example
%% keywords. The predicate is either passed with the validator or is the
%% value of the element being validated.
%% This makes it possible to use a hidden input element with the predicate
%% name as value and class 'nosubmit'.
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

-module(validator_admin_hasedge).

-export([
    render_validator/5,
    validate/5,
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

render_validator(hasedge, TriggerId, TargetId, Args, Context)  ->
    {_PostbackJS, PostbackInfo} = z_render:make_postback({validate, Args}, 'postback', TriggerId, TargetId, ?MODULE, Context),
    ValidatorArgs = z_utils:js_object(
        z_validation:rename_args([ {z_postback, PostbackInfo} | Args ])
    ),
    Script = [
        <<"z_add_validator(\"">>,TriggerId,<<"\", \"postback\", ">>, ValidatorArgs, <<");\n">>
    ],
    {Args, Script}.

validate(hasedge, _TriggerId, Password, _Args, Context) ->
    {{ok, Password}, Context}.

event(#postback{message={validate, Args}, trigger=TriggerId}, Context) ->
    Id = proplists:get_value(id, Args),
    Value = z_context:get_q(<<"triggervalue">>, Context),
    Predicate = proplists:get_value(predicate, Args, Value),
    Min = z_convert:to_integer(proplists:get_value(minimum, Args, 1)),
    Max = z_convert:to_integer(proplists:get_value(maximum, Args, undefined)),
    Count = length(m_edge:objects(Id, Predicate, Context)),
    IsValid = Count >= Min andalso (Max =:= undefined orelse Count =< Max),
    z_render:add_script(iolist_to_binary([
        "z_async_validation_result(",
            "'", TriggerId, "',",
            z_convert:to_binary(IsValid), ",",
            "'", z_utils:js_escape(Value), "'",
         ");"
    ]), Context).
