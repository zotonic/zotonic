%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Zotonic
%% @doc Tests for editable translation fields, source languages and update patches.
%% @end

%% Copyright 2026 Zotonic
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

-module(translation_edit_tests).
-author("Marc Worrell <marc@worrell.nl>").
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

exact_language_test() ->
    Raw = #{<<"title">> => #trans{tr = [{en, <<"A &amp; B">>}] }},
    {ok, #{text := <<"A & B">>, type := input}} = translation_edit:field(Raw, undefined, <<"title">>, en),
    {ok, #{text := <<>>}} = translation_edit:field(Raw, undefined, <<"title">>, nl).

shared_text_properties_test() ->
    TextProperties = translation_translate_rsc:text_properties(),
    ShortProperties = translation_translate_rsc:short_text_properties(),
    ?assertEqual(length(lists:usort(TextProperties)), length(TextProperties)),
    lists:foreach(fun(Property) ->
        ?assert(lists:member(Property, TextProperties)),
        ?assertEqual(input, translation_edit:editor_type(Property))
    end, ShortProperties),
    lists:foreach(fun(Property) ->
        ?assert(translation_translate_rsc:is_text(Property)),
        ?assertMatch({ok, #{text := <<"Text">>}}, translation_edit:field(
            #{Property => <<"Text">>}, undefined, Property, en))
    end, [<<"intro_html">> | TextProperties]),
    ?assertNot(translation_translate_rsc:is_text(<<"title_slug">>)),
    ?assertNot(translation_translate_rsc:is_text(<<"website">>)),
    ?assertEqual({error, field}, translation_edit:field(
        #{<<"website">> => <<"https://example.test">>}, undefined, <<"website">>, en)).

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

source_languages_test() ->
    Raw = #{<<"language">> => [en, nl, de, fr], <<"title">> => #trans{tr = [
        {en, <<"Source">>}, {nl, <<"Target">>}, {de, <<"  ">>}, {es, <<"Not enabled">>}]}},
    ?assertEqual([en], translation_edit:source_languages(Raw, undefined, <<"title">>, nl)),
    ?assertEqual([], translation_edit:source_languages(Raw, undefined, <<"title_slug">>, nl)),
    ?assertEqual([en], translation_edit:source_languages(Raw#{<<"title">> => <<"Source">>},
        undefined, <<"title">>, nl)),
    Block = #{<<"name">> => <<"intro">>, <<"body">> => #trans{tr = [{en, <<"<p>Body</p>">>}]}},
    ?assertEqual([en], translation_edit:source_languages(Raw#{<<"blocks">> => [Block]},
        <<"intro">>, <<"body">>, nl)),
    ?assertEqual([], translation_edit:source_languages(Raw#{<<"blocks">> => [Block, Block]},
        <<"intro">>, <<"body">>, nl)).

patch_test() ->
    ?assertEqual(#{<<"title">> => <<"Text">>}, translation_edit:patch(undefined, <<"title">>, <<"Text">>)),
    ?assertEqual(#{<<"blocks">> => [#{<<"name">> => <<"intro">>, <<"body">> => <<"Text">>}]},
        translation_edit:patch(<<"intro">>, <<"body">>, <<"Text">>)).
