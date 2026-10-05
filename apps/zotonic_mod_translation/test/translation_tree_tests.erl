%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Integration tests for tree translation, permissions, and sidejob progress.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(translation_tree_tests).
-author("Marc Worrell <marc@worrell.nl>").
-moduledoc("Integration tests use temporary resources and remove them even after failure.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([run/1, translate/2]).

%% @doc Run the tree integration tests on an existing site using temporary resources.
run(Site) ->
    Context = z_acl:sudo(z_context:new(list_to_existing_atom(Site))),
    eunit:test(tests(Context), [verbose]).

%% @doc Create the EUnit test for the standard test sandbox site.
tree_test_() -> tests(z_acl:sudo(z_context:new(zotonic_site_testsandbox))).

%% @doc Wrap the integration test with a timeout for background job checks.
tests(Context) ->
    {timeout, 60, fun() -> integration(Context) end}.

%% @doc Verify tree operations, permissions, progress, and binary text translation with temporary fixtures.
integration(Context) ->
    Root = insert([en, nl], Context),
    A = insert([en, nl], Context),
    B = insert([nl], Context),
    try
        {ok, Root} = m_rsc:update(Root, #{<<"menu">> => [#rsc_tree{id = A, tree = [#rsc_tree{id = B}]}, #rsc_tree{id = A}]}, Context),
        {ok, _} = m_edge:insert(A, haspart, Root, Context),
        {ok, Ids} = m_translation_tree:ids(Root, Context),
        ?assertEqual(lists:sort([Root, A, B]), Ids),
        {ok, #{languages := Counts, total := 3}} = m_translation_tree:details(Root, Context),
        ?assertEqual([{en, 2}, {nl, 3}], Counts),
        Anonymous = z_context:new(z_context:site(Context)),
        ?assertMatch({error, _}, m_translation_tree:m_get([Root], undefined, Anonymous)),
        ?assertMatch({error, _}, m_translation_tree:m_post([Root], #{payload => #{<<"method">> => <<"remove">>, <<"language">> => <<"nl">>, <<"confirmed">> => true}}, Anonymous)),
        ?assertEqual({error, badarg}, m_translation_tree:m_post([Root], #{payload => #{<<"method">> => <<"remove">>, <<"language">> => <<"nl">>}}, Context)),
        ?assertEqual({error, badarg}, m_translation_tree:m_post([Root], #{payload => #{
            <<"method">> => <<"copy">>, <<"src">> => <<"en">>, <<"dst">> => <<"nl">>
        }}, Context)),
        {ok, _} = translation_tree:start(Root, {remove, nl}, Context),
        Removed = await(Root, Context, 500),
        ?assertMatch(#{state := complete, done := 3, skipped := 1, failed := 0}, Removed),
        ?assertEqual([en], m_rsc:p(A, language, Context)),
        ?assertEqual([nl], m_rsc:p(B, language, Context)),
        {ok, _} = translation_tree:start(Root, {<<"copy">>, en, nl, false}, Context),
        Copied = await(Root, Context, 500),
        ?assertMatch(#{state := complete, done := 3, skipped := 1, failed := 0}, Copied),
        ?assertEqual(<<"Tree translation test text 47">>, proplists:get_value(nl, (m_rsc:p(A, title, Context))#trans.tr)),
        {ok, A} = m_rsc:update(A, #{<<"title">> => #trans{tr = [{en, <<"Tree translation test text 47">>}, {nl, <<"Keep me">>}]}}, Context),
        ok = translation_translate_rsc:copy_translation(A, en, nl, false, Context),
        ?assertEqual(<<"Keep me">>, proplists:get_value(nl, (m_rsc:p(A, title, Context))#trans.tr)),
        ok = translation_translate_rsc:copy_translation(A, en, nl, true, Context),
        ?assertEqual(<<"Tree translation test text 47">>, proplists:get_value(nl, (m_rsc:p(A, title, Context))#trans.tr)),
        %% Exercise a real in-flight job without calling a remote translation service.
        z_notifier:observe(translate, {?MODULE, translate}, self(), 1, Context),
        SlowContext = z_context:set(translation_tree_test, {wait, self()}, Context),
        StartedAt = erlang:monotonic_time(millisecond),
        {ok, #{state := running, total := 0}} = translation_tree:start(Root, {<<"translate">>, en, nl, true}, SlowContext),
        ?assert(erlang:monotonic_time(millisecond) - StartedAt < 2000),
        Worker = receive {translating, Pid} -> Pid after 2000 -> error(no_translation) end,
        ?assertMatch(#{state := running}, translation_tree:status(Root, Context)),
        %% Clearing the cache must not lose authoritative progress for a live worker.
        z_depcache:flush({translation_tree, Root}, Context),
        ?assertMatch(#{state := running}, translation_tree:status(Root, Context)),
        %% Restore the last progress snapshot to also exercise recovery after worker death.
        z_depcache:set({translation_tree, Root}, translation_tree:status(Root, Context), Context),
        ?assertEqual({error, busy}, translation_tree:start(Root, {remove, nl}, Context)),
        %% Only the UI is locked: normal server-side edits and edge updates still work.
        ?assertEqual({ok, A}, m_rsc:update(A, #{<<"summary">> => <<"Editable">>}, Context)),
        ?assertMatch({ok, _}, m_edge:insert(Root, relation, A, Context)),
        exit(Worker, kill),
        wait_unlocked(Root, Context, 100),
        ?assertMatch(#{state := failed}, translation_tree:status(Root, Context)),
        %% Traversal includes visible read-only pages, but processing skips them before
        %% calling the translation service (even though the tree was already collected).
        ReadOnlyContext = SlowContext#context{acl = undefined, acl_is_read_only = true},
        {ok, Ids} = m_translation_tree:ids(Root, ReadOnlyContext),
        {ok, _} = translation_tree:start(Root, {<<"translate">>, en, nl, true}, ReadOnlyContext),
        ?assertMatch(#{state := complete, failed := 0, skipped := 3}, await(Root, Context, 500)),
        receive {translating, _} -> error(translated_read_only_page) after 0 -> ok end,
        IncompleteContext = z_context:set(translation_tree_test, incomplete, Context),
        {ok, _} = translation_tree:start(Root, {<<"translate">>, en, nl, true}, IncompleteContext),
        ?assertMatch(#{state := complete, failed := 2, skipped := 1}, await(Root, Context, 500)),
        ?assertEqual(<<"Tree translation test text 47">>, proplists:get_value(nl, (m_rsc:p(A, title, Context))#trans.tr)),
        z_notifier:detach(translate, self(), Context),
        {ok, _} = translation_tree:start(Root, {<<"empty">>, nl, en, false}, Context),
        ?assertMatch(#{state := complete, failed := 0, skipped := 0}, await(Root, Context, 500)),
        ?assertEqual([en, nl], m_rsc:p(B, language, Context)),
        %% Fields without source content must remain valid translation records.
        {ok, A} = m_rsc:update(A, #{<<"body">> => #trans{tr = [{nl, <<"Only Dutch">>}]}}, Context),
        ok = translation_translate_rsc:copy_translation(A, en, nl, true, Context),
        ?assertEqual(<<"Only Dutch">>, proplists:get_value(nl, (m_rsc:p(A, body, Context))#trans.tr)),
        %% Translate plain binary fields using the page's source language, even when
        %% the editor context has a different language. Nested block fields follow
        %% the same allowlist; identifiers and arbitrary binary data stay untouched.
        BinaryText = <<"Nederlandse brontekst voor boomvertaling 47">>,
        TextKeys = [<<"title">>, <<"short_title">>, <<"chapeau">>, <<"summary">>,
            <<"body">>, <<"body_extra">>, <<"date_remarks">>, <<"prompt">>,
            <<"explanation">>, <<"matching">>, <<"narrative">>, <<"feedback">>,
            <<"seo_title">>, <<"seo_desc">>, <<"seo_keywords">>, <<"custom_html">>],
        BinaryProps = maps:from_list([{K, BinaryText} || K <- TextKeys]),
        Block = BinaryProps#{<<"type">> => <<"text">>,
            <<"name">> => <<"unchanged">>, <<"data">> => BinaryText,
            <<"data_json">> => <<"{}">>},
        {ok, B} = m_rsc:update(B, BinaryProps#{<<"language">> => [nl],
            <<"blocks">> => [Block]}, Context),
        ?assertEqual(BinaryText, m_rsc:p(B, prompt, Context)),
        [StoredBlock] = m_rsc:p(B, blocks, Context),
        ?assertEqual(BinaryText, maps:get(<<"prompt">>, StoredBlock)),
        z_notifier:observe(translate, {?MODULE, translate}, self(), 1, Context),
        BinaryContext = z_context:set(translation_tree_test, binary_text,
            z_context:set_language(en, Context)),
        {ok, _} = translation_tree:start(Root, {<<"translate">>, nl, en, true}, BinaryContext),
        ?assertMatch(#{state := complete, failed := 0, skipped := 0}, await(Root, Context, 500)),
        [TranslatedBlock] = m_rsc:p(B, blocks, Context),
        lists:foreach(fun(K) ->
            Expected = #trans{tr = [{en, <<"Translated: ", BinaryText/binary>>}, {nl, BinaryText}]},
            ?assertEqual(Expected, m_rsc:p(B, K, Context)),
            ?assertEqual(Expected, maps:get(K, TranslatedBlock))
        end, TextKeys),
        ?assertEqual(<<"unchanged">>, maps:get(<<"name">>, TranslatedBlock)),
        ?assertEqual(BinaryText, maps:get(<<"data">>, TranslatedBlock)),
        ?assertEqual(<<"{}">>, maps:get(<<"data_json">>, TranslatedBlock)),
        z_notifier:detach(translate, self(), Context),
        %% Compile the core templates, including the dialog and editor hooks.
        {ok, Details} = m_translation_tree:details(Root, Context),
        Vars = #{id => Root, tree_id => Root, tree => Details},
        lists:foreach(fun(Template) ->
            ?assertMatch({ok, _}, z_template:template_module(list_to_binary(Template), Vars, Context))
        end, ["_dialog_translation_tree.tpl", "_translation_tree_init.tpl", "_translation_tree_button.tpl",
            "_translation_edit_languages.tpl", "_admin_edit_sidebar.tpl", "_admin_frontend_edit.tpl"])
    after
        z_notifier:detach(translate, self(), Context),
        case z_proc:whereis({translation_tree, Root}, Context) of
            undefined -> ok;
            JobPid -> exit(JobPid, kill), wait_unlocked(Root, Context, 100)
        end,
        [m_rsc:delete(Id, Context) || Id <- [Root, A, B]]
    end.

%% @doc Create a temporary text resource with source text in each supplied language.
insert(Langs, Context) ->
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => text, <<"language">> => Langs,
        <<"title">> => #trans{tr = [{L, <<"Tree translation test text 47">>} || L <- Langs]}}, Context),
    Id.

%% @doc Wait for terminal job progress and registry cleanup, failing when retries are exhausted.
await(_, _, 0) -> error(timeout);
await(Root, Context, N) ->
    case translation_tree:status(Root, Context) of
        #{state := running} -> timer:sleep(20), await(Root, Context, N - 1);
        State -> wait_unlocked(Root, Context, 100), State
    end.

%% @doc Wait for the worker to release its tree registration, with a bounded retry count.
wait_unlocked(_, _, 0) -> error(lock_timeout);
wait_unlocked(Root, Context, N) ->
    case z_proc:whereis({translation_tree, Root}, Context) of
        undefined -> ok;
        _ -> timer:sleep(10), wait_unlocked(Root, Context, N - 1)
    end.

%% @doc Stub translation service responses only for requests bearing this test's context marker.
translate(#translate{texts = Texts}, Context) ->
    case z_context:get(translation_tree_test, Context) of
        {wait, Caller} ->
            Caller ! {translating, self()},
            receive continue -> {ok, Texts} end;
        incomplete -> {ok, [undefined || _ <- Texts]};
        binary_text -> {ok, [<<"Translated: ", Text/binary>> || Text <- Texts]};
        _ -> undefined
    end.
