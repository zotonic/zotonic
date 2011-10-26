%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Validator for checking if an input value matches a regular expression

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

-module(validator_base_format).
-include("zotonic.hrl").
-export([render_validator/5, validate/5]).

render_validator(format, TriggerId, _TargetId, Args, Context)  ->
    Pattern  = proplists:get_value(pattern, Args),
    Negate   = proplists:get_value(negate, Args, false),
	JsObject = z_utils:js_object(z_validation:rename_args(Args)),
	Script   = [<<"z_add_validator(\"">>,TriggerId,<<"\", \"format\", ">>, JsObject, <<");\n">>],
	{[z_utils:is_true(Negate),Pattern], Script, Context}.


%% @spec validate(Type, TriggerId, Value, Args, Context) -> {ok,AcceptedValue} | {error,Id,Error}
%%          Error = invalid | novalue | {script, Script}
validate(format, _Id, [], _, Context) ->
    {{ok, []}, Context};
validate(format, Id, Value, [Negate,Pattern], Context) ->
    PcrePattern = javascript_to_pcre_pattern(Pattern),
    {Re,Options} = pattern_to_re(PcrePattern),

    %% If a unicode pattern is given, convert the utf8 list into a unicode char list
    {Value1, Options1} = case z_string:contains("\\u", Pattern) of
			     true ->
				 {unicode:characters_to_list(erlang:list_to_binary(Value)), [unicode|Options]};
			     false ->
				 {Value, Options}
			 end,
		 
    Ok = not Negate,
    Match = case re:run(Value1, Re, Options1) of
                {match, _} -> true;
                nomatch -> false
            end,

    case Match of
        Ok -> {{ok, Value}, Context};
        _  -> {{error, Id, invalid}, Context}
    end.
    
%% @doc Translate a regular expression in javascript format to erlang re module format
pattern_to_re([$/|Rest]=Pattern) ->
    case string:rchr(Rest, $/) of
        0 -> 
            {Pattern,[]};
        N -> 
            {Re, [$/|Options]} = lists:split(N-1,Rest),
            ReOptions = [anycrlf|trans_options(Options, [])],
            {Re, ReOptions}
    end;
pattern_to_re(Pattern) ->
    {Pattern, []}.
    
trans_options([], Acc) -> 
    Acc;
trans_options([$i|T], Acc) -> 
    trans_options(T, [caseless|Acc]);
trans_options([$m|T], Acc) -> 
    trans_options(T, [multiline|Acc]);
trans_options([$s|T], Acc) -> 
    trans_options(T, [dotall|Acc]);
trans_options([$x|T], Acc) -> 
    trans_options(T, [extended|Acc]);
trans_options([$A|T], Acc) -> 
    trans_options(T, [anchored|Acc]);
trans_options([$D|T], Acc) -> 
    trans_options(T, [dollar_endonly|Acc]);
trans_options([$U|T], Acc) -> 
    trans_options(T, [ungreedy|Acc]);
trans_options([_|T], Acc) -> 
    trans_options(T, Acc).

%% @doc Make a javascript regular expression pcre compatible.
javascript_to_pcre_pattern(Pattern) ->    
    %% convert \uXXXX to \x{XXXX}
    R1 = re:replace(Pattern, "([\\\\]{1}[u]{1}[0-9a-fA-F]{4,6})", "@@--@@&}", [global]),
    re:replace(R1, "@@--@@[\\\\]u", "\\\\x{", [{return, list}, global]).
