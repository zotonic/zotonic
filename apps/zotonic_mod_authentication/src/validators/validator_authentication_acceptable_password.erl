%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2024-2025 Marc Worrell
%% @doc Check if a password is acceptable for the site password restrictions.
%% @end

%% Copyright 2024-2025 Marc Worrell
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
-moduledoc("
A [validator](/id/doc_developerguide_forms_and_validation#guide-validators) to check whether a password conforms to the
password secutiry requirements.

It can be attached to a password entry field used to set a new password:


```django
<input type=\"password\" id=\"password\" name=\"password\" autocomplete=\"new-password\" value=\"\">
{% validate id=\"password\"
            type={acceptable_password}
            only_on_blur
%}
```

The password will be sent to the server when the user leaves the password field.

The server will then check for:

*   minimum password length, as configured in `mod_authentication.password_min_length` (defaults to 8)
*   matching a regular expression with criteria, as configured in `mod_authentication.password_min_length`
*   check if the password does not appear in a leak, unless `mod_authentication.password_disable_leak_check` is set (this uses the service at [Have I Been Pwned](https://haveibeenpwned.com/Passwords))

You can pass a `failure_message`:


```django
<input type=\"password\" id=\"password\" name=\"password\" autocomplete=\"new-password\" value=\"\">
{% validate id=\"password\"
            type={acceptable_password
                failure_message=_\"Your new password is too short or not strong enough\"
            }
            only_on_blur
%}
```

There is an optional parameter `allow_empty` to allow empty passwords. This is useful if the password should only be
filled in special circumstances (like when changing the password) and the handler code knows not to do anything if the
password is empty:


```django
<label for=\"password\">{_ Optionally set a new password _}</label>
<input type=\"password\" id=\"password\" name=\"password\" autocomplete=\"new-password\" value=\"\">
{% validate id=\"password\"
            type={acceptable_password
                allow_empty
                failure_message=_\"Your new password is too short or not strong enough\"
            }
            only_on_blur
%}
```

See also

[Forms and validation](/id/doc_developerguide_forms_and_validation#guide-validators), [username_unique](/id/doc_template_validator_validator_username_unique)").

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

event(#postback{message={validate, Args}, trigger=TriggerId}, Context) ->
    AllowEmpty = proplists:get_value(allow_empty, Args, false),
    Password = z_convert:to_binary(z_context:get_q(<<"triggervalue">>, Context)),
    IsAcceptable = if
        Password =:= <<>>, AllowEmpty ->
            true;
        true ->
            case m_authentication:acceptable_password(Password, Context) of
                ok -> true;
                {error, _} -> false
            end
    end,
    z_render:add_script(iolist_to_binary([
        "z_async_validation_result(",
            "'", TriggerId, "',",
            z_convert:to_binary(IsAcceptable), ",",
            "'", z_utils:js_escape(Password), "'",
         ");"
    ]), Context).
