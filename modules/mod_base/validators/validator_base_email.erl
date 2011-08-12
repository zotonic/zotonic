%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Based on code by Rusty Klophaus

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

-module(validator_base_email).
-include("zotonic.hrl").
-export([render_validator/5, validate/5, re/0]).

render_validator(email, TriggerId, _TargetId, Args, Context)  ->
	JsObject = z_utils:js_object(z_validation:rename_args(Args)), 
	Script   = [<<"z_add_validator(\"">>,TriggerId,<<"\", \"email\", ">>, JsObject, <<");\n">>],
	{[], Script, Context}.


%% @spec validate(Type, TriggerId, Values, Args, Context) -> {ok,AcceptedValue} | {error,Id,Error}
%%          Error = invalid | novalue | {script, Script}
validate(email, Id, Value, _Args, Context) ->
    case z_string:trim(Value) of
        [] -> {{ok, []}, Context};
        Trimmed ->
            case re:run(Trimmed, [$^|re()]++"$", [extended]) of
                nomatch   -> {{error, Id, invalid}, Context};
                {match,_} -> {{ok, Trimmed}, Context}
            end
	end.


re() ->
    "(
            (\"[^\"\\f\\n\\r\\t\\v\\b]+\")
        |   ([\\w\\!\\#\\$\\%\\&\'\\*\\+\\-\\~\\/\\^\\`\\|\\{\\}]+
                (\\.[\\w\\!\\#\\$\\%\\&\\'\\*\\+\\-\\~\\/\^\`\\|\\{\\}]+)*
            )
    )
    @
    (
        (
            ([A-Za-z0-9\\-])+\\.
        )+
        [A-Za-z\\-]{2,}
    )".

%% Rusty's regexp: "([^@\"\'\\s]+)@(([-a-z0-9]+\\.)+[a-z]{2,})".
%% Complete regexp:
%%   ^((\"[^\"\f\n\r\t\v\b]+\")|([\w\!\#\$\%\&\'\*\+\-\~\/\^\`\|\{\}]+(\.[\w\!\#\$\%\&\'\*\+\-\~\/\^\`\|\{\}]+)*))@((\[(((25[0-5])|(2[0-4][0-9])|([0-1]?[0-9]?[0-9]))\.((25[0-5])|(2[0-4][0-9])|([0-1]?[0-9]?[0-9]))\.((25[0-5])|(2[0-4][0-9])|([0-1]?[0-9]?[0-9]))\.((25[0-5])|(2[0-4][0-9])|([0-1]?[0-9]?[0-9])))\])|(((25[0-5])|(2[0-4][0-9])|([0-1]?[0-9]?[0-9]))\.((25[0-5])|(2[0-4][0-9])|([0-1]?[0-9]?[0-9]))\.((25[0-5])|(2[0-4][0-9])|([0-1]?[0-9]?[0-9]))\.((25[0-5])|(2[0-4][0-9])|([0-1]?[0-9]?[0-9])))|((([A-Za-z0-9\-])+\.)+[A-Za-z\-]+))$
