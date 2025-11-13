%% @author Marc Worrell
%% @hidden

-module(z_template_compile_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").


compile_templates_test_() ->
    {timeout, 30, fun() -> compile_all_templates() end}.

%% Compile all templates in the module index.
compile_all_templates() ->
    Context = z_context:new(zotonic_site_testsandbox),
    Templates = z_module_indexer:all(template, Context),
    lists:foreach(
        fun(#module_index{} = Tpl) ->
            ?assertMatch(
                {{ok, _},_},
                {z_template:template_module(Tpl, #{}, Context), Tpl})
        end, Templates).
