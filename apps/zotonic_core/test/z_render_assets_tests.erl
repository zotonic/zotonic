%% @hidden
-module(z_render_assets_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").

assets_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        fun page_duplicates/0,
        fun materialized_context/0,
        fun update_dependencies/0,
        fun spaceless_structures/0,
        fun identities/0,
        fun empty_script/0,
        fun model_response/0,
        fun compiled_template/0,
        fun collection_order/0,
        fun collection_type_isolation/0,
        fun collection_options/0,
        fun collection_boundaries/0,
        fun collection_validation/0,
        fun compiled_collect/0
    ]}.

setup() ->
    meck:new(z_lib_include, [passthrough, no_link]),
    meck:expect(z_lib_include, tag, fun(Files, _Args, _C) ->
        [[<<"<asset>">>, File, <<"</asset>">>] || File <- Files]
    end),
    meck:expect(z_lib_include, assets, fun(Files, _Args, _C) ->
        [#{key => F, url => F, type => js} || F <- Files]
    end),
    meck:new(m_config, [no_link]),
    meck:expect(m_config, get_boolean, fun(M, K, C) ->
        maps:get({M, K}, z_context:get(test_config, C, #{}), false)
    end),
    meck:new(z_notifier, [no_link]),
    meck:expect(z_notifier, map, fun(_, _) -> [] end),
    meck:expect(z_notifier, notify, fun(_, _) -> ok end),
    ok.

cleanup(_) ->
    meck:unload([z_lib_include, z_notifier, m_config]).

lib(Files) -> {lib, Files, []}.

page_duplicates() ->
    Tree = [lib([<<"a.js">>]), [lib([<<"a.js">>, <<"b.js">>])]],
    Expected = <<"<asset>a.js</asset><asset>b.js</asset>">>,
    {Html, _} = z_render:output(Tree, #context{}),
    ?assertEqual(Expected, iolist_to_binary(Html)),
    %% Reusing a cached render tree on another page starts with fresh state.
    {Again, _} = z_render:output(Tree, #context{}),
    ?assertEqual(Expected, iolist_to_binary(Again)),
    {AcrossScript, _} = z_render:output(
        [lib([<<"a.js">>]), {script, []}, lib([<<"a.js">>])], z_context:set_csp_nonce(<<"test">>, #context{})),
    ?assertEqual(1, length(binary:matches(iolist_to_binary(AcrossScript), <<"<asset>">>))).

materialized_context() ->
    {First, C1} = z_render:render_to_iolist(lib([<<"a.js">>]), #context{}),
    {Second, _} = z_render:render_to_iolist(lib([<<"a.js">>, <<"b.js">>]), C1),
    ?assertEqual(<<"<asset>a.js</asset>">>, iolist_to_binary(First)),
    ?assertEqual(<<"<asset>b.js</asset>">>, iolist_to_binary(Second)).

update_dependencies() ->
    Result = z_render:render_with_assets([
        <<"<div>update</div>">>, lib([<<"a.js">>, <<"a.js">>]),
        {javascript, <<"initialize();">>}
    ], #context{}),
    ?assertEqual(<<"<div>update</div>">>, maps:get(html, Result)),
    ?assertMatch([#{key := <<"a.js">>}], maps:get(libs, Result)),
    ?assertNotEqual(nomatch, binary:match(maps:get(javascript, Result), <<"initialize();">>)),
    C = z_render:update(<<"target">>, [lib([<<"a.js">>]), <<"<div></div>">>], #context{}),
    Script = iolist_to_binary(z_render:get_script(C)),
    ?assertMatch(<<"z_assets.run(", _/binary>>, Script),
    ?assertEqual(nomatch, binary:match(Script, <<"<asset>">>)),
    ?assertNotEqual(nomatch, binary:match(Script, <<"widgetManager">>)).

spaceless_structures() ->
    ScriptState = z_context:get_render_state(z_render:add_script(<<"init();">>, #context{})),
    Tree = z_template_compiler_runtime:spaceless_tag([
        <<"  <div> </div> \n ">>, lib([<<"a.js">>]), ScriptState,
        <<" <p>ok</p>  ">>
    ], #{}, #context{}),
    {Html, _} = z_render:output([Tree], #context{}),
    ?assertEqual(<<"<div></div><asset>a.js</asset><p>ok</p>">>, iolist_to_binary(Html)),
    Result = z_render:render_with_assets(Tree, #context{}),
    ?assertEqual(<<"<div></div><p>ok</p>">>, maps:get(html, Result)),
    ?assertNotEqual(nomatch, binary:match(maps:get(javascript, Result), <<"init();">>)).

identities() ->
    {Files, Seen} = z_lib_include:filter([<<"/a.js">>, <<"a.js">>, <<"x.css">>, <<"x.png">>], [], #{}, #context{}),
    ?assertEqual([<<"x.css">>, <<"a.js">>], Files),
    {[], _} = z_lib_include:filter([<<"a.js">>, <<"x.css">>], [], Seen, #context{}),
    {[<<"x.css">>], _} = z_lib_include:filter([<<"x.css">>], [{media, <<"print">>}], Seen, #context{}).

empty_script() ->
    ?assertEqual([], z_render:get_script(#context{})).

model_response() ->
    meck:new(z_template, [no_link]),
    meck:expect(z_template, render, fun(<<"_test.tpl">>, [], _) ->
        [<<"<p>test</p>">>, lib([<<"a.js">>]), {javascript, <<"init();">>}]
    end),
    meck:expect(z_template, render_to_iolist, fun(<<"_test.tpl">>, [], C) -> {<<"legacy">>, C} end),
    try
        ?assertMatch({ok, {#{html := <<"<p>test</p>">>, libs := [_], javascript := _}, []}},
            m_template:m_get([<<"render_with_assets">>, <<"_test.tpl">>], #{}, #context{})),
        ?assertEqual({ok, {<<"legacy">>, []}},
            m_template:m_get([<<"render">>, <<"_test.tpl">>], #{}, #context{}))
    after
        meck:unload(z_template)
    end.

%% Exercise the compiler/runtime boundary, including a nested spaceless block.
compiled_template() ->
    {ok, _} = application:ensure_all_started(template_compiler),
    Template = <<"{% spaceless %}<p>one</p> \n"
                 "{% lib \"a.js\" %}{% spaceless %} <p>two</p> {% endspaceless %}"
                 "{% lib \"a.js\" \"b.js\" %}{% endspaceless %}">>,
    {ok, Module} = template_compiler:compile_binary(Template,
        <<"z_render_assets_tests.tpl">>, [{runtime, z_template_compiler_runtime}], #context{}),
    Tree = Module:render(#{}, #{}, #context{}),
    {Html, _} = z_render:render_to_iolist(Tree, #context{}),
    ?assertEqual(<<"<p>one</p><asset>a.js</asset><p>two</p><asset>b.js</asset>">>,
        iolist_to_binary(Html)).

collect(Files) -> {lib, Files, [collect]}.
flush(Type) -> {lib, [], [{flush, Type}]}.

materialize(Tree) ->
    {Html, _} = z_render:render_to_iolist(Tree, #context{}),
    iolist_to_binary(Html).

collection_order() ->
    ?assertEqual(<<"startmiddle<asset>a.js</asset><asset>b.js</asset>end">>,
        materialize([<<"start">>, collect([<<"a.js">>]), <<"middle">>,
                     lib([<<"b.js">>]), <<"end">>])),
    %% An ordinary duplicate is still a flush point; repeated collect is not.
    ?assertEqual(<<"middle<asset>a.js</asset>end">>,
        materialize([collect([<<"a.js">>]), collect([<<"a.js">>]), <<"middle">>,
                     lib([<<"a.js">>]), <<"end">>])),
    ?assertEqual(<<"<asset>a.css</asset>middle<asset>a.js</asset>end">>,
        materialize([collect([<<"a.js">>, <<"a.css">>]), flush(<<"css">>),
                     <<"middle">>, flush(<<"javascript">>), <<"end">>])),
    ?assertEqual(<<>>, materialize([flush(true), flush(<<"js">>)])),
    %% The reusable (cached) tree has no page-specific pending/seen state.
    Tree = [collect([<<"a.js">>]), flush(true)],
    ?assertEqual(materialize(Tree), materialize(Tree)).

%% Ordinary tags flush only the types present in their original file list.
collection_type_isolation() ->
    ?assertEqual(
        <<"<asset>a.css</asset><asset>b.css</asset>middle<asset>a.js</asset>end">>,
        materialize([collect([<<"a.css">>, <<"a.js">>]),
            lib([<<"b.css">>]), <<"middle">>, flush(<<"js">>), <<"end">>])),
    ?assertEqual(
        <<"<asset>a.js</asset><asset>b.js</asset>middle<asset>a.css</asset>end">>,
        materialize([collect([<<"a.css">>, <<"a.js">>]),
            lib([<<"b.js">>]), <<"middle">>, flush(<<"css">>), <<"end">>])),
    %% Deduplication must not broaden the flush to the other pending type.
    ?assertEqual(
        <<"<asset>a.css</asset>middle<asset>a.js</asset>end">>,
        materialize([collect([<<"a.css">>, <<"a.js">>]),
            lib([<<"a.css">>]), <<"middle">>, flush(true), <<"end">>])),
    ?assertEqual(
        <<"<asset>a.js</asset>middle<asset>a.css</asset>end">>,
        materialize([collect([<<"a.css">>, <<"a.js">>]),
            lib([<<"a.js">>]), <<"middle">>, flush(true), <<"end">>])).

collection_options() ->
    meck:reset(z_lib_include),
    materialize([{lib, [<<"a.css">>], [collect, {media, <<"screen">>}]},
                 {lib, [<<"b.css">>], [collect, {media, <<"print">>}]},
                 {lib, [<<"c.css">>], [collect, {media, <<"screen">>}]}, flush(true)]),
    ?assertEqual(3, meck:num_calls(z_lib_include, tag, '_')),
    ?assert(meck:called(z_lib_include, tag, [[<<"b.css">>], [{media, <<"print">>}], '_'])),
    meck:reset(z_lib_include),
    materialize([collect([<<"a.js">>]), lib([<<"b.js">>])]),
    ?assert(meck:called(z_lib_include, tag, [[<<"a.js">>, <<"b.js">>], [], '_'])),
    meck:reset(z_lib_include),
    materialize([collect([<<"a.js">>]), {lib, [<<"b.js">>], [async]}]),
    ?assertEqual(2, meck:num_calls(z_lib_include, tag, '_')),
    ?assert(meck:called(z_lib_include, tag, [[<<"a.js">>], [], '_'])),
    ?assert(meck:called(z_lib_include, tag, [[<<"b.js">>], [async], '_'])),
    %% Effective minification, not just the explicit boolean, determines compatibility.
    C = z_context:set(test_config, #{{site, minification_enabled} => true}, #context{}),
    ?assertEqual(z_lib_include:options_key(js, [], C), z_lib_include:options_key(js, [minify], C)),
    ?assertNotEqual(z_lib_include:options_key(js, [], C), z_lib_include:options_key(js, [nocache], C)),
    ?assertNotEqual(z_lib_include:options_key(js, [], C), z_lib_include:options_key(js, [absolute_url], C)),
    ?assertNotEqual(z_lib_include:options_key(css, [], C),
                    z_lib_include:options_key(css, [{rel, <<"alternate stylesheet">>}], C)),
    ?assertNotEqual(z_lib_include:options_key(css, [], C),
                    z_lib_include:options_key(css, [{title, <<"alternate">>}], C)).

collection_boundaries() ->
    %% Nested lists/spaceless must not flush; the final materialization must.
    ?assertEqual(<<"<b>x</b>after<asset>a.js</asset>">>,
        materialize([{spaceless, [collect([<<"a.js">>]), <<" <b>x</b> ">>], []}, <<"after">>])),
    C = z_context:set_csp_nonce(<<"test">>, #context{}),
    {Page, _} = z_render:output([collect([<<"a.js">>]), {script, []}], C),
    ?assertMatch(<<"<asset>a.js</asset>", _/binary>>, iolist_to_binary(Page)),
    {Js, _} = z_render:output([collect([<<"a.js">>]), {script, [{format, <<"js">>}]}], C),
    ?assertEqual(nomatch, binary:match(iolist_to_binary(Js), <<"<asset>">>)),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Js), <<"z_assets.run">>)),
    Result = z_render:render_with_assets([collect([<<"a.js">>]),
        <<"<p>update</p>">>, {javascript, <<"init();">>}], #context{}),
    ?assertEqual(<<"<p>update</p>">>, maps:get(html, Result)),
    ?assertMatch([#{key := <<"a.js">>}], maps:get(libs, Result)),
    %% A queued context can also be finalized directly as update JavaScript.
    Queued = z_render:render(collect([<<"a.js">>]), #context{}),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(z_render:get_script(Queued)), <<"a.js">>)).

collection_validation() ->
    lists:foreach(fun(Option) ->
        ?assertError({invalid_lib_options, collect_with_async_or_defer},
            materialize([{lib, [<<"a.js">>], [collect, Option]}]))
    end, [async, defer]),
    ?assertError({invalid_lib_options, flush_with_files_or_options},
        materialize([{lib, [<<"a.js">>], [flush]}])),
    ?assertError({invalid_lib_options, flush_with_files_or_options},
        materialize([{lib, [], [flush, {media, <<"print">>}]}])),
    ?assertError({invalid_lib_options, {flush, <<"invalid">>}}, materialize([flush(<<"invalid">>)])),
    ?assertError({invalid_lib_options, missing_files}, materialize([lib([])])),
    ?assertMatch({include, true, _}, z_lib_include:operation([<<"a.js">>], [collect, {async, false}])).

compiled_collect() ->
    Template = <<"{% lib \"a.js\" collect %}before{% lib flush=\"js\" %}after">>,
    {ok, Module} = template_compiler:compile_binary(Template,
        <<"z_render_collect_tests.tpl">>, [{runtime, z_template_compiler_runtime}], #context{}),
    ?assertEqual(<<"before<asset>a.js</asset>after">>, materialize(Module:render(#{}, #{}, #context{}))).
