%% Copyright 2026 Zotonic. Apache-2.0.
-module(translation_edit_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

exact_language_test() ->
    Raw = #{<<"title">> => #trans{tr = [{en, <<"A &amp; B">>}] }},
    {ok, #{text := <<"A & B">>, type := input}} = translation_edit:field(Raw, undefined, <<"title">>, en),
    {ok, #{text := <<>>}} = translation_edit:field(Raw, undefined, <<"title">>, nl).

field_types_test() ->
    ?assertEqual(html, translation_edit:editor_type(<<"body_extra">>)),
    ?assertEqual(html, translation_edit:editor_type(<<"intro_html">>)),
    ?assertEqual(textarea, translation_edit:editor_type(<<"summary">>)),
    {ok, #{text := <<"<p>Hello</p>">>}} = translation_edit:field(
        #{<<"body">> => <<"<p>Hello</p>">>}, undefined, <<"body">>, en).

block_target_test() ->
    Block = #{<<"name">> => <<"intro">>, <<"body">> => <<"Text">>},
    Raw = #{<<"blocks">> => [Block]},
    ?assertMatch({ok, _}, translation_edit:field(Raw, <<"intro">>, <<"body">>, en)),
    ?assertEqual({error, block}, translation_edit:field(Raw, <<>>, <<"body">>, en)),
    ?assertEqual({error, block}, translation_edit:field(Raw, <<"missing">>, <<"body">>, en)),
    ?assertEqual({error, block}, translation_edit:field(
        Raw#{<<"blocks">> => [Block, Block]}, <<"intro">>, <<"body">>, en)).

unsafe_and_structured_test() ->
    ?assertEqual({error, field}, translation_edit:field(
        #{<<"body_unsafe">> => #trans{tr = []}}, undefined, <<"body_unsafe">>, en)),
    ?assertEqual({error, field}, translation_edit:field(
        #{<<"answers">> => [#{<<"value">> => <<"yes">>}]}, undefined, <<"answers">>, en)).

custom_slug_test() ->
    Raw = #{<<"title_slug">> => #trans{tr = [{en, <<"test-page">>}]}},
    ?assertEqual({error, field}, translation_edit:field(Raw, undefined, <<"title_slug">>, en)),
    ?assertEqual({error, field}, translation_edit:field(
        Raw#{<<"custom_slug">> => false}, undefined, <<"title_slug">>, en)),
    ?assertMatch({ok, #{type := input, text := <<"test-page">>}}, translation_edit:field(
        Raw#{<<"custom_slug">> => true}, undefined, <<"title_slug">>, en)),
    ?assertMatch({ok, #{type := input}}, translation_edit:field(
        #{<<"custom_slug">> => true, <<"title_slug">> => <<"test-page">>},
        undefined, <<"title_slug">>, en)).

patch_test() ->
    ?assertEqual(#{<<"title">> => <<"Text">>}, translation_edit:patch(undefined, <<"title">>, <<"Text">>)),
    ?assertEqual(#{<<"blocks">> => [#{<<"name">> => <<"intro">>, <<"body">> => <<"Text">>}]},
        translation_edit:patch(<<"intro">>, <<"body">>, <<"Text">>)).
