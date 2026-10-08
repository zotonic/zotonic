%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Test SPARQL result formats, escaping and unbound values.
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

-module(z_sparql_results_encode_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").

csv_test() ->
    Doc = document([<<"uri">>, <<"blank">>, <<"text">>, <<"number">>, <<"missing">>, <<"empty">>], [#{
        <<"uri">> => term(<<"uri">>, <<"https://example.org/a">>),
        <<"blank">> => term(<<"bnode">>, <<"b1">>),
        <<"text">> => (term(<<"literal">>, <<"Hello, \"world\"\r\n", 233/utf8>>))#{<<"xml:lang">> => <<"en">>},
        <<"number">> => (term(<<"literal">>, <<"42">>))#{<<"datatype">> => xsd(<<"integer">>)},
        <<"empty">> => term(<<"literal">>, <<>>)}]),
    ?assertEqual(<<"\"uri\",\"blank\",\"text\",\"number\",\"missing\",\"empty\"\r\n",
        "\"https://example.org/a\",\"_:b1\",\"Hello, \"\"world\"\"\r\n", 233/utf8, "\",\"42\",,\r\n">>, encode(csv, Doc)).

tsv_test() ->
    Doc = document([<<"uri">>, <<"text">>, <<"number">>, <<"blank">>, <<"empty">>, <<"missing">>], [#{
        <<"uri">> => term(<<"uri">>, <<"https://example.org/a">>),
        <<"text">> => (term(<<"literal">>, <<"a\t\r\n\"\\", 233/utf8>>))#{<<"xml:lang">> => <<"nl">>},
        <<"number">> => (term(<<"literal">>, <<"42">>))#{<<"datatype">> => xsd(<<"integer">>)},
        <<"blank">> => term(<<"bnode">>, <<"b1">>),
        <<"empty">> => (term(<<"literal">>, <<>>))#{<<"datatype">> => xsd(<<"string">>)}}]),
    ?assertEqual(<<"?uri\t?text\t?number\t?blank\t?empty\t?missing\n",
        "<https://example.org/a>\t\"a\\t\\r\\n\\\"\\\\", 233/utf8,
        "\"@nl\t\"42\"^^<http://www.w3.org/2001/XMLSchema#integer>\t_:b1\t\"\"\t\n">>, encode(tsv, Doc)),
    Escaped = encode(tsv, document([<<"v">>], [#{<<"v">> => term(<<"uri">>, <<"https://example.org/a>\\\t">>)}])),
    ?assertEqual(<<"?v\n<https://example.org/a\\u003E\\u005C\\u0009>\n">>, Escaped).

xml_test() ->
    Text = <<"<tag>&\"'\n\t", 233/utf8>>,
    Doc = document([<<"text">>, <<"number">>, <<"uri">>, <<"blank">>, <<"missing">>, <<"empty">>], [#{
        <<"text">> => (term(<<"literal">>, Text))#{<<"xml:lang">> => <<"nl">>},
        <<"number">> => (term(<<"literal">>, <<"42">>))#{<<"datatype">> => xsd(<<"integer">>)},
        <<"uri">> => term(<<"uri">>, <<"https://example.org/?a=1&b=2">>),
        <<"blank">> => term(<<"bnode">>, <<"b1">>),
        <<"empty">> => term(<<"literal">>, <<>>)}]),
    {Xml, []} = xmerl_scan:string(binary_to_list(encode(xml, Doc)), [{namespace_conformant, true}]),
    ?assertEqual('http://www.w3.org/2005/sparql-results#', (Xml#xmlElement.namespace)#xmlNamespace.default),
    [#xmlText{value = Actual}] = xmerl_xpath:string("/sparql/results/result/binding[@name='text']/literal/text()", Xml),
    ?assertEqual(Text, unicode:characters_to_binary(Actual)),
    [#xmlAttribute{value = "nl"}] = xmerl_xpath:string("/sparql/results/result/binding[@name='text']/literal/@xml:lang", Xml),
    [#xmlAttribute{value = Datatype}] = xmerl_xpath:string("/sparql/results/result/binding[@name='number']/literal/@datatype", Xml),
    ?assertEqual(xsd(<<"integer">>), list_to_binary(Datatype)),
    ?assertEqual([], xmerl_xpath:string("/sparql/results/result/binding[@name='missing']", Xml)),
    ?assertEqual(5, length(xmerl_xpath:string("/sparql/results/result/binding", Xml))),
    % xmerl normalizes CR character references; verify their encoding directly.
    ?assertNotEqual(nomatch, binary:match(encode(xml, document([<<"v">>],
        [#{<<"v">> => term(<<"literal">>, <<"a\r\nb">>)}])), <<"a&#13;&#10;b">>)),
    ?assertThrow({error, unsupported_result_term}, encode(xml,
        document([<<"v">>], [#{<<"v">> => term(<<"literal">>, <<0>>)}]))).

empty_results_test() ->
    Doc = document([<<"a">>, <<"b">>], []),
    ?assertEqual(<<"\"a\",\"b\"\r\n">>, encode(csv, Doc)),
    ?assertEqual(<<"?a\t?b\n">>, encode(tsv, Doc)),
    {Xml, []} = xmerl_scan:string(binary_to_list(encode(xml, Doc))),
    ?assertEqual(2, length(xmerl_xpath:string("/sparql/head/variable", Xml))),
    ?assertEqual([], xmerl_xpath:string("/sparql/results/result", Xml)),
    ?assertEqual(Doc, z_json:decode(encode(json, Doc))),
    ?assertEqual(<<"\"a\",\"b\"\r\n,\r\n">>, encode(csv, document([<<"a">>, <<"b">>], [#{}]))),
    ?assertEqual(<<"?a\t?b\n\t\n">>, encode(tsv, document([<<"a">>, <<"b">>], [#{}]))).

document(Vars, Rows) -> #{<<"head">> => #{<<"vars">> => Vars}, <<"results">> => #{<<"bindings">> => Rows}}.
term(Type, Value) -> #{<<"type">> => Type, <<"value">> => Value}.
xsd(Name) -> <<"http://www.w3.org/2001/XMLSchema#", Name/binary>>.
encode(csv, Doc) -> z_sparql_results_encode:encode({<<"text">>, <<"csv">>, []}, Doc);
encode(tsv, Doc) -> z_sparql_results_encode:encode({<<"text">>, <<"tab-separated-values">>, []}, Doc);
encode(xml, Doc) -> z_sparql_results_encode:encode({<<"application">>, <<"sparql-results+xml">>, []}, Doc);
encode(json, Doc) -> z_sparql_results_encode:encode({<<"application">>, <<"sparql-results+json">>, []}, Doc).

csv_exact_values_test() ->
    Value = <<"=formula\t", 1, ",\"quoted\"">>,
    Doc = document([<<"v">>], [#{<<"v">> => term(<<"literal">>, Value)}]),
    ?assertEqual(<<"\"v\"\r\n\"=formula\t", 1, ",\"\"quoted\"\"\"\r\n">>, encode(csv, Doc)),
    % Existing CSV consumers keep spreadsheet protection and control filtering.
    ?assertEqual(z_csv_writer:encode_line([Value], $,), z_csv_writer:encode_line([Value], $,, #{})),
    ?assertEqual(<<"\"'=formula,\"\"quoted\"\"\"\r\n">>, z_csv_writer:encode_line([Value], $,)).
