%% @author Marc Worrell
%% @hidden

-module(z_template_compile_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").


compile_templates_test_() ->
    Context = z_context:new(zotonic_site_testsandbox),
    Templates = z_module_indexer:all(template, Context),
    % Bound each compilation, not the entire collection: cold CI runs can take
    % more than 30 seconds in total. The path identifies failures and timeouts.
    [
        {z_convert:to_list(Tpl#module_index.filepath),
            {timeout, 30, fun() -> ?assertMatch(
                {{ok, _},_},
                {z_template:template_module(Tpl, #{}, Context), Tpl})
            end}}
        || #module_index{} = Tpl <- Templates
    ].
