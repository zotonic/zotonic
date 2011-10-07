-module(mod_admin_tests).

-include_lib("eunit/include/eunit.hrl").
-include("zotonic.hrl").

wire_update_test() ->
    Context = z_context:new(testsandbox),
    z_ids:fix_seed(),
    {Html, ContextHtml} = z_template:render_to_iolist("tests/wire_update_test.tpl", [], Context),
    _Script = z_script:get_script(ContextHtml),
    %% ?assertEqual(<<"\n\n$(\"#summary\").html(\"\\x3cbutton id\\x3d\\x27mtbuaa\\x27\\x3eClick Me!\\x3c/button\\x3e\\n\").widgetManager();\n$(\"#mtbuaa\").bind('click', function(event) { alert(\"Thank you\"); return z_opt_cancel(this); } );\n">>,
    %%              iolist_to_binary(Script)),

    ?assertEqual(<<"<div id=\"summary\">Bla bla bla</div>\n\n">>, iolist_to_binary(Html)),
    ok.

%     <div id="summary">Bla bla bla</div>
%     {% wire action={update selector="#summary" template="_test.tpl"} %}
% en in _test.tpl dan dit zet:
% Een button:
%    {% button text="Click Me!" action={alert text="Thank you"} %}
   
