%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009  Marc Worrell
%% @doc Handle parameter validation of a request. Checks for the presence 
%% of z_v elements containing validation information.

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

-module(z_validation).
-export([validate_query_args/1]).

-include_lib("zotonic.hrl").

%% @todo Translate unique id-names to base names (after validation)   #name -> fghw-name in postback+qs -> name in validated result

%% @spec validate_query_args(Context) -> {ok, NewContext} | {error, NewContext}
%% @doc Checks for z_v arguments, performs enclosed checks and adds the validated terms to the q_validated list.
%%      Errors are reported back to the user agent
validate_query_args(Context) ->
    case z_context:get(q_validated, Context) of
        undefined ->
            Validations = z_context:get_q_all("z_v", Context),
            Validated   = lists:map(fun(X) -> validate(X,Context) end, Validations),

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

            Context1 = z_context:set(q_validated, QsValidated, Context),
            Context2 = report_errors(Errors, Context1),
            
            case Errors of
                [] -> {ok, Context2};
                _  -> {error, Context2}
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
    Script   = [<<"z_validation_error('">>, ErrId, <<"', \"">>, z_utils:js_escape(atom_to_list(Error)),<<"\");\n">>],
    Context1 = z_script:add_script(Script, Context),
    report_errors(T, Context1).


%% @doc Perform all validations
validate(Val, Context) ->
    [Name,Pickled]        = string:tokens(Val, ":"),
    {Id,Name,Validations} = z_utils:depickle(Pickled, Context),
    Value                 = z_context:get_q(Name, Context),

    %% Fold all validations, propagate errors
    ValidateF = fun
                    (_Validation,{error, _, _}=Error) -> Error;
                    (Validation,{ok, V}) ->
                        case Validation of
                            {Type,Module,Args} -> Module:validate(Type, Id, V, Args, Context);
                            {Type,Module}      -> Module:validate(Type, Id, V, [], Context)
                        end
                end,
    Validated = lists:foldl(ValidateF, {ok,Value}, Validations),
    {Name, Validated}.
