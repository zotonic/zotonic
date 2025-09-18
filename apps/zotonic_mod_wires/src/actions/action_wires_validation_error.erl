%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Original code copyright (c) 2008-2009 Rusty Klophaus
%% @todo Adapt this for Zotonic.

-module(action_wires_validation_error).
-moduledoc("
Render a validation error on the target. Text is given in the text argument.

Todo

Extend documentation
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
