%% @author Michael Connors <michael@bring42.net>
%% @copyright 2012 Michael Connors
%%

%% Copyright 2012 Michael Connors
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

-module(validator_base_date).
-include("zotonic.hrl").
-export([render_validator/5, validate/5]).

render_validator(date, TriggerId, _TargetId, Args, Context)  ->
    Format = proplists:get_value(format, Args, "l"),
    Separator = proplists:get_value(separator, Args, "-"),
    JsObject = z_utils:js_object(z_validation:rename_args(Args)), 
    Script   = [<<"z_add_validator(\"">>,TriggerId,<<"\", \"date\", ">>, JsObject, <<");\n">>],
    {[Format, Separator], Script, Context}.

is_valid(Day, Month, Year) ->
    calendar:valid_date(list_to_integer(Year), list_to_integer(Month), list_to_integer(Day)).
is_valid(Day, Month, Year, "l") ->
    is_valid(Day, Month, Year);
is_valid(Month, Day, Year, "m") ->
    is_valid(Day, Month, Year);
is_valid(Year, Month, Day, "b") ->
    is_valid(Day, Month, Year);
is_valid(_, _, _, _) ->
    false.


%% @spec validate(Type, TriggerId, Values, Args, Context) -> {ok,AcceptedValue} | {error,Id,Error}
%%          Error = invalid | novalue | {script, Script}
validate(date, Id, Value, [Format, Separator], Context) ->
    case lists:member(Format, ["l", "m", "b"]) of
        true ->
            case z_string:trim(Value) of
                [] -> {{ok, []}, Context};
                Trimmed ->
                    case list_to_tuple(string:tokens(Trimmed, Separator)) of
                        {Part1, Part2, Part3} ->
                            case is_valid(Part1, Part2, Part3, Format) of
                                true ->
                                    {{ok, Trimmed}, Context};
                                false ->
                                    {{error, Id, invalid}, Context}
                            end;
                        _Others   -> {{error, Id, invalid}, Context}
                    end
            end;
         false ->
             {{error, Id, invalid}, Context}
    end.