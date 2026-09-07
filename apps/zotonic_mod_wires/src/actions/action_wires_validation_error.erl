%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Original code copyright (c) 2008-2009 Rusty Klophaus
%% @todo Adapt this for Zotonic.

-module(action_wires_validation_error).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "wire_action", "forms", "user_interface_and_interaction", "validate"]
}).
-moduledoc("
Render a validation error on the target. Text is given in the text argument.

The `text` argument supplies the failure message.

::: note
This action is an incomplete legacy implementation and still needs to be
adapted for Zotonic's current validation JavaScript. It constructs a
`LiveValidation` validator around an implicit `me` object and should not be used
as the basis for new validation code.
:::
").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).


render_action(_TriggerId, _TargetId, Args, Context) ->
	Text   = z_utils:js_escape(proplists:get_value(text,Args,<<>>)),
	Script = [
		    <<"var v = new LiveValidation(obj('me'), { onlyOnSubmit: true }); ">>,
		    <<"v.add(Validate.Custom, { against: wf_return_false, failureMessage: \"">>,Text,<<"~s\" }); ">>,
		    <<"v.validate(); ">>
	        ],
	{Script,Context}.
