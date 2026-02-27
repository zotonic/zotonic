%% @doc Validator for checking page_path uniqueness
%% @end

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
-module(validator_base_page_path_unique).
-moduledoc("
A [validator](/id/doc_developerguide_forms_and_validation#guide-validators) to check whether a resource’s page path is unique:


```django
<input type=\"text\" id=\"page_path\" name=\"page_path\" value=\"\">
{% validate id=\"page_path\" type={page_path_unique} %}
```

Optionally, pass an `id` parameter to exclude that particular id when testing for uniqueness. This is useful when you
want to exclude the page paths of the resource currently being edited:


```django
<input type=\"text\" id=\"page_path\" name=\"page_path\" value=\"\">
{% validate id=\"page_path\" type={page_path_unique id=id} %}
```

You can also pass a `failure_message`:


```django
<input type=\"text\" id=\"page_path\" name=\"page_path\" value=\"\">
{% validate id=\"page_path\" type={page_path_unique id=id failure_message=_\"Eek! Already used!\"} %}
```

See also

[Forms and validation](/id/doc_developerguide_forms_and_validation#guide-validators), [username_unique](/id/doc_template_validator_validator_username_unique), [name_unique](/id/doc_template_validator_validator_name_unique)").

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    render_validator/5,
    validate/5,
    event/2
]).

render_validator(page_path_unique, TriggerId, TargetId, Args, Context)  ->
    {_PostbackJS, PostbackInfo} = z_render:make_postback({validate, Args}, 'postback', TriggerId, TargetId, ?MODULE, Context),
    JsObject = z_utils:js_object(z_validation:rename_args([{z_postback, PostbackInfo} | Args])),
    Script = [<<"z_add_validator(\"">>, TriggerId, <<"\", \"postback\", ">>, JsObject, <<");\n">>],
    {Args, Script}.

-spec validate(page_path_unique, binary(), term(), list(), z:context()) ->
    {{ok, binary()}, z:context()} | {{error, m_rsc:resource(), atom() | binary()}, #context{}}.
validate(page_path_unique, Id, Value, Args, Context) ->
    Value1 = z_string:trim(z_convert:to_binary(Value)),
    case z_utils:is_empty(Value1) of
        true ->
            {{ok, <<>>}, Context};
        false ->
            NormPath = m_rsc_update:normalize_page_path(Value1),
            RscId = proplists:get_value(id, Args),
            case m_rsc:page_path_to_id(NormPath, Context) of
                {ok, RscId} ->
                    {{ok, <<>>}, Context};
                {ok, _} ->
                    Message = proplists:get_value(failure_message, Args, invalid),
                    {{error, Id, Message}, Context};
                {redirect, _} ->
                    {{ok, <<>>}, Context};
                {error, _} ->
                    {{ok, <<>>}, Context}
            end
    end.

%% @doc Handle the validation during form entry.
-spec event(#postback{}, z:context()) -> z:context().
event(#postback{message = {validate, Args}, trigger = TriggerId}, Context) ->
    Value = z_context:get_q(triggervalue, Context),
    {IsValid, ContextValidated} = case validate(page_path_unique, TriggerId, Value, Args, Context) of
        {{ok, _}, ContextOk} ->
            {"true", z_render:wire({fade_out, [{target, <<TriggerId/binary, "_path_unique_error">>}]}, ContextOk)};
        {{error, Id, _} = Error, ContextScript} ->
            {"false", z_render:wire({fade_in, [{target, <<TriggerId/binary, "_path_unique_error">>}]},
                z_validation:report_errors([{Id, Error}], ContextScript))}
    end,
    z_render:add_script(
        ["z_async_validation_result('", TriggerId, "', ", IsValid, ", '", z_utils:js_escape(Value), "');"],
        ContextValidated
    ).
