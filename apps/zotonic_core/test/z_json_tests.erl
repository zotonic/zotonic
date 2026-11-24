%% @author Marc Worrell
%% @copyright 2026 Marc Worrell
%% @doc JSON encoding regression tests.
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

-module(z_json_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

malformed_export_test() ->
    Body = <<"<p>Old", 160, "text</p>">>,
    Export = #{<<"status">> => <<"ok">>, <<"result">> => #{
        <<"resource">> => #{<<"managed_prop">> => #{<<"body">> => Body}},
        <<"title">> => <<"Café"/utf8, 16#a0/utf8, 16#1f600/utf8>>}},
    Json = z_json:encode(Export),
    #{<<"result">> := Result} = z_json:decode(Json),
    ?assertEqual(<<"<p>Old", 16#fffd/utf8, "text</p>">>,
        maps:get(<<"body">>, maps:get(<<"managed_prop">>, maps:get(<<"resource">>, Result)))),
    ?assertEqual(maps:get(<<"title">>, maps:get(<<"result">>, Export)), maps:get(<<"title">>, Result)),
    ?assertNotEqual(nomatch, binary:match(Json, <<"<\\/p>">>)).

malformed_strings_test_() ->
    [?_assertEqual(Expected, z_json:decode(z_json:encode(Input))) || {Input, Expected} <- [
        {<<160>>, <<16#fffd/utf8>>},
        {<<"prefix", 226, 130>>, <<"prefix", 16#fffd/utf8>>},
        {<<255, "ok", 160>>, <<16#fffd/utf8, "ok", 16#fffd/utf8>>},
        {<<"valid", 0, 16#a0/utf8, 16#1f600/utf8>>, <<"valid", 0, 16#a0/utf8, 16#1f600/utf8>>},
        {<<>>, <<>>}
    ]].

nested_types_test() ->
    Input = #{<<"key", 160>> => [#{value => <<255>>},
        #trans{tr = [{en, <<"Text", 160>>}]}, {{2026, 11, 24}, {12, 0, 0}}, 42, true, undefined]},
    Repaired = #{<<"key", 16#fffd/utf8>> => [#{value => <<16#fffd/utf8>>},
        #trans{tr = [{en, <<"Text", 16#fffd/utf8>>}]}, {{2026, 11, 24}, {12, 0, 0}}, 42, true, undefined]},
    ?assertEqual(z_json:decode(z_json:encode(Repaired)), z_json:decode(z_json:encode(Input))).
