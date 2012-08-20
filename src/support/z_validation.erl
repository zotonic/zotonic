%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2010  Marc Worrell
%% @doc Handle parameter validation of a request. Checks for the presence 
%% of z_v elements containing validation information.

%% Copyright 2009-2010 Marc Worrell
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

-module(z_validation).
-export([
    validate_query_args/1,
    report_errors/2,
    rename_args/1,
    get_q/2
]).

-include_lib("zotonic.hrl").

%% @doc Rename validator arguments to names that are compatible with the LiveValidation plugin.
%% @spec rename_args(PropList) -> PropList1
rename_args(Args) ->
    rename_args(Args, []).

rename_args([], Acc) ->
    Acc;
rename_args([{failure_message, Msg}|T], Acc) ->
    rename_args(T, [{failureMessage, Msg}|Acc]);
rename_args([{valid_message, Msg}|T], Acc) ->
    rename_args(T, [{validMessage, Msg}|Acc]);
rename_args([{not_a_number_message, Msg}|T], Acc) ->
    rename_args(T, [{notANumberMessage, Msg}|Acc]);
rename_args([{not_an_integer_message, Msg}|T], Acc) ->
    rename_args(T, [{notAnIntegerMessage, Msg}|Acc]);
rename_args([{wrong_number_message, Msg}|T], Acc) ->
    rename_args(T, [{wrongNumberMessage, Msg}|Acc]);
rename_args([{too_low_message, Msg}|T], Acc) ->
    rename_args(T, [{tooLowMessage, Msg}|Acc]);
rename_args([{too_high_message, Msg}|T], Acc) ->
    rename_args(T, [{tooHighMessage, Msg}|Acc]);
rename_args([{too_short_message, Msg}|T], Acc) ->
    rename_args(T, [{tooShortMessage, Msg}|Acc]);
rename_args([{too_long_message, Msg}|T], Acc) ->
    rename_args(T, [{tooLongMessage, Msg}|Acc]);
rename_args([{wrong_length_message, Msg}|T], Acc) ->
    rename_args(T, [{wrongLengthMessage, Msg}|Acc]);
rename_args([{partial_match, Msg}|T], Acc) ->
    rename_args(T, [{partialMatch, Msg}|Acc]);
rename_args([{case_sensitive, Msg}|T], Acc) ->
    rename_args(T, [{caseSensitive, Msg}|Acc]);
rename_args([{allow_null, Msg}|T], Acc) ->
    rename_args(T, [{allowNull, Msg}|Acc]);
rename_args([{only_on_submit, Value}|T], Acc) ->
    rename_args(T, [{onlyOnSubmit, Value}|Acc]);
rename_args([{only_on_blur, Value}|T], Acc) ->
    rename_args(T, [{onlyOnBlur, Value}|Acc]);
rename_args([H|T], Acc) ->
    rename_args(T, [H|Acc]).



%% @todo Translate unique id-names to base names (after validation)   #name -> fghw-name in postback+qs -> name in validated result

%% @spec validate_query_args(Context) -> {ok, NewContext} | {error, NewContext}
%% @doc Checks for z_v arguments, performs enclosed checks and adds the validated terms to the q_validated list.
%%      Errors are reported back to the user agent
validate_query_args(Context) ->
    case z_context:get(q_validated, Context) of
        undefined ->
            Validations = z_context:get_q_all("z_v", Context),
            {Validated,Context1} = lists:foldl(
                                            fun(X, {Acc, Ctx}) -> 
                                                {XV, Ctx1} = validate(X,Ctx),
                                                {[XV|Acc], Ctx1}
                                            end,
                                            {[], Context},
                                            Validations),

            % format is like: [{"email",{ok,"me@example.com"}}]
            % Grep all errors, make scripts for the context var
            % Move all ok values to the q_validated dict
            IsError  = fun 
                            ({_Id, {error, _, _}}) -> true;
                            (_X) -> false
                       end,
            GetValue = fun
                            ({Id, {ok, #upload{} = Value}}) -> {Id, Value};
                            ({Id, {ok, Value}}) -> {Id, lists:flatten(Value)}
                       end,

            {Errors,Values} = lists:partition(IsError, Validated),
            QsValidated     = lists:map(GetValue, Values),

            Context2 = z_context:set(q_validated, QsValidated, Context1),
            Context3 = report_errors(Errors, Context2),
            
            case Errors of
                [] -> {ok, Context3};
                _  -> {error, Context3}
            end;
        _ ->
            {ok, Context}
    end.


%% @doc Add all errors as javascript message to the request result.
report_errors([], Context) -> 
    Context;
report_errors([{_Id, {error, _ErrId, {script, Script}}}|T], Context) ->
    Context1 = z_script:add_script(Script, Context),
    report_errors(T, Context1);
report_errors([{_Id, {error, ErrId, Error}}|T], Context) ->
    Script   = [<<"z_validation_error('">>, ErrId, <<"', \"">>, z_utils:js_escape(z_convert:to_list(Error)),<<"\");\n">>],
    Context1 = z_script:add_script(Script, Context),
    report_errors(T, Context1).


%% @doc Perform all validations
validate(Val, Context) ->
    [Name,Pickled] = string:tokens(Val, ":"),
    {Id,Name1,Validations} = z_utils:depickle(Pickled, Context),
    Name = z_convert:to_list(Name1),
    Value = case [ V || V <- z_context:get_q_all(Name, Context), V =/= [], V =/= <<>> ] of
                [A] -> A;
                Vs -> Vs
            end,

    %% Fold all validations, stop on error
    ValidateF = fun
                    (_Validation,{{error, _, _}=Error, Ctx}) -> {Error, Ctx};
                    (Validation,{{ok, V}, Ctx}) ->
                        case Validation of
                            {Type,Module,Args} -> Module:validate(Type, Id, V, Args, Ctx);
                            {Type,Module}      -> Module:validate(Type, Id, V, [], Ctx)
                        end
                end,
    {Validated, Context1} = lists:foldl(ValidateF, {{ok,Value}, Context}, Validations),
    {{Name, Validated}, Context1}.


%% @doc Simple utility function to get the 'q' value of an argument. When the argument has a generated unique prefix then
%% the prefix is stripped.
get_q(Name, Context) ->
    case z_context:get_q(Name, Context) of
        undefined ->
            case string:tokens(Name, "-") of
                [_Prefix, Name1] -> z_context:get_q(Name1, Context);
                _ -> undefined
            end;
        Value -> Value
    end.

