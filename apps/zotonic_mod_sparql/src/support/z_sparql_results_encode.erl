%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Encode SPARQL SELECT results as JSON, XML, CSV or TSV.
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

-module(z_sparql_results_encode).
-moduledoc("SPARQL result encoders sharing the endpoint's normalized RDF bindings.").

-export([encode/2]).

%% Formats: https://www.w3.org/TR/sparql11-results-csv-tsv/
%%          https://www.w3.org/TR/rdf-sparql-XMLres/
-spec encode(ContentType, Document) -> binary()
    when ContentType :: cowmachine_req:media_type(), Document :: map().
encode({<<"application">>, Type, _}, Document)
    when Type =:= <<"json">>; Type =:= <<"sparql-results+json">> ->
    z_json:encode(Document);
encode({<<"application">>, <<"sparql-results+xml">>, _}, Document) ->
    iolist_to_binary(xml(Document));
encode({<<"text">>, <<"csv">>, _}, Document) ->
    iolist_to_binary(csv(Document));
encode({<<"text">>, <<"tab-separated-values">>, _}, Document) ->
    iolist_to_binary(table(Document, fun tsv_term/1, fun(Name) -> [$?, Name] end, <<"\t">>, <<"\n">>)).

table(#{<<"head">> := #{<<"vars">> := Vars}, <<"results">> := #{<<"bindings">> := Rows}},
        Term, Header, Separator, Eol) ->
    [[lists:join(Separator, [Header(V) || V <- Vars]), Eol] |
        [[lists:join(Separator, [Term(maps:get(V, Row, undefined)) || V <- Vars]), Eol] || Row <- Rows]].

%% CSV deliberately drops datatype/language information. Empty strings and
%% unbound variables have the same representation in SPARQL Results CSV.
csv(#{<<"head">> := #{<<"vars">> := Vars}, <<"results">> := #{<<"bindings">> := Rows}}) ->
    Options = #{sanitize => false},
    [z_csv_writer:encode_line(Vars, $,, Options) |
        [z_csv_writer:encode_line([csv_term(maps:get(V, Row, undefined)) || V <- Vars], $,, Options)
         || Row <- Rows]].

csv_term(undefined) -> <<>>;
csv_term(#{<<"type">> := <<"bnode">>, <<"value">> := Value}) -> <<"_:", Value/binary>>;
csv_term(#{<<"value">> := Value}) -> Value.

tsv_term(undefined) -> <<>>;
tsv_term(#{<<"type">> := <<"uri">>, <<"value">> := Value}) -> iri(Value);
tsv_term(#{<<"type">> := <<"bnode">>, <<"value">> := Value}) -> [<<"_:">>, Value];
tsv_term(#{<<"type">> := <<"literal">>, <<"value">> := Value} = Binding) ->
    Literal = [$", escape(Value, literal), $"],
    case Binding of
        #{<<"xml:lang">> := Language} -> [Literal, $@, Language];
        #{<<"datatype">> := <<"http://www.w3.org/2001/XMLSchema#string">>} -> Literal;
        #{<<"datatype">> := Datatype} -> [Literal, <<"^^">>, iri(Datatype)];
        _ -> Literal
    end.

iri(Value) -> [$<, escape(Value, iri), $>].

xml(#{<<"head">> := #{<<"vars">> := Vars}, <<"results">> := #{<<"bindings">> := Rows}}) ->
    [<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
       "<sparql xmlns=\"http://www.w3.org/2005/sparql-results#\"><head>">>,
     [[<<"<variable name=\"">>, escape(V, xml), <<"\"/>">>] || V <- Vars],
     <<"</head><results>">>,
     [[<<"<result>">>, [xml_binding(V, maps:get(V, Row, undefined)) || V <- Vars], <<"</result>">>] || Row <- Rows],
     <<"</results></sparql>\n">>].

xml_binding(_Name, undefined) -> [];
xml_binding(Name, Binding) ->
    [<<"<binding name=\"">>, escape(Name, xml), <<"\">">>, xml_term(Binding), <<"</binding>">>].

xml_term(#{<<"type">> := <<"uri">>, <<"value">> := Value}) ->
    [<<"<uri>">>, escape(Value, xml), <<"</uri>">>];
xml_term(#{<<"type">> := <<"bnode">>, <<"value">> := Value}) ->
    [<<"<bnode>">>, escape(Value, xml), <<"</bnode>">>];
xml_term(#{<<"type">> := <<"literal">>, <<"value">> := Value} = Binding) ->
    Attribute = case Binding of
        #{<<"xml:lang">> := Language} -> [<<" xml:lang=\"">>, escape(Language, xml), $"];
        #{<<"datatype">> := Datatype} -> [<<" datatype=\"">>, escape(Datatype, xml), $"];
        _ -> []
    end,
    [<<"<literal">>, Attribute, $>, escape(Value, xml), <<"</literal>">>].

%% Work on Unicode codepoints. XML character references preserve CR and
%% attribute whitespace; forbidden XML 1.0 characters cannot be represented.
escape(Value, Mode) -> [escape_char(C, Mode) || <<C/utf8>> <= Value].

escape_char($&, xml) -> <<"&amp;">>;
escape_char($<, xml) -> <<"&lt;">>;
escape_char($>, xml) -> <<"&gt;">>;
escape_char($", xml) -> <<"&quot;">>;
escape_char($', xml) -> <<"&apos;">>;
escape_char($\t, xml) -> <<"&#9;">>;
escape_char($\n, xml) -> <<"&#10;">>;
escape_char($\r, xml) -> <<"&#13;">>;
escape_char(C, xml) when C < 32; C =:= 16#FFFE; C =:= 16#FFFF ->
    throw({error, unsupported_result_term});
escape_char($", literal) -> <<"\\\"">>;
escape_char($\\, literal) -> <<"\\\\">>;
escape_char($\t, literal) -> <<"\\t">>;
escape_char($\n, literal) -> <<"\\n">>;
escape_char($\r, literal) -> <<"\\r">>;
escape_char($\b, literal) -> <<"\\b">>;
escape_char($\f, literal) -> <<"\\f">>;
escape_char(C, iri) when C =< 32; C =:= $<; C =:= $>; C =:= $";
        C =:= ${; C =:= $}; C =:= $|; C =:= $^; C =:= $`; C =:= $\\ ->
    unicode_escape(C);
escape_char(C, literal) when C < 32 -> unicode_escape(C);
escape_char(C, _Mode) -> <<C/utf8>>.

unicode_escape(C) -> io_lib:format("\\u~4.16.0B", [C]).
