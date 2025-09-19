%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2010 Marc Worrell
%% @doc Add a validation to an element

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

-module(scomp_wires_validate).
-moduledoc("
The validator tag accepts the following arguments:

| Argument           | Description                                                                      | Example                                 |
| ------------------ | -------------------------------------------------------------------------------- | --------------------------------------- |
| id                 | The id of the input element to be validated.                                     | id=”password\\\\_field”                   |
| type               | The validator for the input element. Can be specified multiple times to add multiple validators. The value depends on the validator choosen but is always a record `{validatorname arg=value ...}` | type=\\\\{acceptance\\\\}                   |
| trigger            | Id of the element that triggers the validation. Defaults to the value of “id”. The validator is triggered by a change of this. Almost never used. | trigger=”title”                         |
| target             | Target of validator, defaults to the trigger. Almost never used.                 |                                         |
| name               | Use this when the name of the input element is unequal to the id. The name is used by the server side code to validate the received input. Defaults to the target argument (which defaults to id). | name=”password\\\\_field”                 |
| valid\\\\_message    | Message to show when the field passes validation. Defaults to the empty string.  | valid\\\\_message=”ok!”                   |
| failure\\\\_message  | Argument passed to the type argument. See individual validators.                 |                                         |
| message\\\\_after    | The id of the element after which the failure message should be shown. Defaults to the id argument. | message\\\\_after= “signup\\\\_tos\\\\_agree” |
| only\\\\_on\\\\_blur   | Normally validates on change, unles only\\\\_on\\\\_blur is set.                     | only\\\\_on\\\\_blur                        |
| wait               | Time in msec to wait for validation after the last keystroke. Default: 0.        | wait=100                                |
| only\\\\_on\\\\_submit | Whether the validation should be done when entering data or only on submit of the form. Set this to suppress validation when entering data. | only\\\\_on\\\\_submit                      |

See also

*   the list of [Validators](/id/template_validator#validators)
*   [Forms and validation](/id/doc_developerguide_forms_and_validation#guide-validators) in the Developer Guide
").
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(zotonic_scomp).
-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.

render(Params, _Vars, Context) ->
    Id       = proplists:get_value(id, Params, <<>>),
    TargetId = proplists:get_value(target,Params,Id),
    case proplists:get_all_values(on_form_invalid, Params) of
        [] ->
            {ok, z_render:validator(Id, TargetId, z_validation:rename_args(Params), Context)};
        OnInvalid ->
            ContextWire = lists:foldl(
                                fun (Act, Ctx) ->
                                    z_render:wire(Act, Ctx)
                                end,
                                z_context:new(Context),
                                lists:flatten(OnInvalid)),
            Script = iolist_to_binary(z_render:get_script(ContextWire)),
            {ok, z_render:add_script(["z_validation_on_invalid('",Id,"', function() {",Script,"});"], Context)}
   end.
