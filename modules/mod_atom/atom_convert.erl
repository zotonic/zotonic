%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2009-10-02
%% @doc OAuth.

%% Copyright 2009 Arjan Scherpenisse
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

-module(atom_convert).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([resource_to_atom/1,
         atom_to_resource/1
        ]).


-include_lib("xmerl/include/xmerl.hrl").
-include_lib("zotonic.hrl").

-define(ATOM_NS, 'http://www.w3.org/2005/Atom').

%% @doc Export a resource to Atom XML.
%% @spec resource_to_atom(rsc_export()) -> string()
resource_to_atom(RscExport) ->
    Rsc = proplists:get_value(rsc, RscExport),
    Content0 = [
                {id, [binary_to_list(proplists:get_value(uri, RscExport))]},
                {title, [{type, "text"}], [binary_to_list(proplists:get_value(title, Rsc))]},
                {published, [z_convert:to_isotime(proplists:get_value(publication_start, Rsc))]},
                {updated, [z_convert:to_isotime(proplists:get_value(modified, Rsc))]}
               ],

    Content1 = case empty(Body = proplists:get_value(body, Rsc)) of
                   true ->
                       Content0;
                   false ->
                       Content0 ++ [{content, [{type, "html"}], [binary_to_list(Body)]}]
               end,

    Content2 = case empty(Summary = proplists:get_value(summary, Rsc)) of
                   true ->
                       Content1;
                   false ->
                       Content1 ++ [{summary, [{type, "text"}], [binary_to_list(Summary)]}]
               end,

    Content3 = Content2 ++ author_element(RscExport),

    RootElem = #xmlElement{name=entry, 
                           namespace=#xmlNamespace{default=?ATOM_NS},
                           attributes=[#xmlAttribute{name=xmlns, value=?ATOM_NS}],
                           content=Content3},
    lists:flatten(xmerl:export_simple([RootElem], xmerl_xml)).


%% @doc Construct the Atom author element.
%% @spec author_element(rsc_export()) -> [#xmlElement{}]
author_element(Export) ->
    case proplists:get_value(edges, Export) of
        X when X =:= undefined orelse X =:= [] ->
            [];
        Edges ->
            [#xmlElement{name=author, content=[
                         #xmlElement{name=name, content=[#xmlText{value=proplists:get_value(object_title, E)}]},
                         #xmlElement{name=uri, content=[#xmlText{value=proplists:get_value(object_uri, E)}]}]}
             || E <- filter_edges(Edges, <<"author">>)]
    end.


%% @doc Given a list of edges, filter out the ones which have the given predicate name.
%% @spec filter_edges([edge()], atom()) -> [edge()]
filter_edges(Edges, PredicateName) ->
    lists:filter(fun(X) -> proplists:get_value(predicate_name, X) == PredicateName end, Edges).



%% @doc Export a resource to Atom XML.
%% @spec atom_to_resource(string()) -> rsc_export()
atom_to_resource(Xml) when is_binary(Xml) ->
    atom_to_resource(binary_to_list(Xml));

atom_to_resource(Xml) ->
    {RootElem,_} = xmerl_scan:string(Xml),

    %% Atom required elements
    RscUri = case xmerl_xpath:string("/entry/id", RootElem) of
                 [] -> undefined;
                 [#xmlElement{content=Uri}] ->
                     list_to_binary(collapse_xmltext(Uri))
             end,

    RscProps1 = case xmerl_xpath:string("/entry/title", RootElem) of
                 [] -> [{title, <<>>}];
                 [Title] ->
                     [{title, get_xmltext(Title, true)}]
             end,

    RscProps2 = case xmerl_xpath:string("/entry/updated", RootElem) of
                 [] -> RscProps1;
                 [#xmlElement{content=Updated}] ->
                     RscProps1 ++ [{modified, z_convert:to_datetime(collapse_xmltext(Updated))}]
             end,

    RscProps3 = case xmerl_xpath:string("/entry/published", RootElem) of
                 [] -> RscProps2;
                 [#xmlElement{content=Published}] ->
                     RscProps2 ++ [{publication_start, z_convert:to_datetime(collapse_xmltext(Published))}]
             end,

    RscProps4 = case xmerl_xpath:string("/entry/summary", RootElem) of
               [] -> RscProps3;
               [Summary] ->
                   RscProps3 ++ [{summary, get_xmltext(Summary, true)}]
           end,

    RscProps5 = case xmerl_xpath:string("/entry/content", RootElem) of
               [] -> RscProps4;
               [Body] ->
                   RscProps4 ++ [{body, get_xmltext(Body, false)}]
           end,

    %% Edges

    Edges = [],

    Edges1 = Edges ++ find_author(RootElem),
    Edges2 = Edges1 ++ find_depiction(RootElem),

    %% Medium

    Medium = case xmerl_xpath:string("/entry/link[@rel=\"enclosure\"]", RootElem) of
               [] -> undefined;
               [Enc] ->
                     [{mime, xml_attrib(type, Enc)},
                      {url, xml_attrib(href, Enc)}]
           end,

    %% Combine all into rsc_export() structure
    Import = [{uri, RscUri},
              {rsc, RscProps5},
              {medium, Medium},
              {edges, Edges2}
             ],
    lists:filter(fun({_,L}) -> not(L == []) end, Import).


%% @doc Given an Atom entry, get a list of all the authors formatted as author edges.
%% @spec find_author(#xmlElement{}) -> [edge()]
find_author(Elem) ->
    case xmerl_xpath:string("/entry/author", Elem) of
        [] -> []; % no author found
        Authors ->
            lists:map(fun(A) ->
                              Name = case xmerl_xpath:string("/author/name", A) of 
                                         [] -> <<>>;
                                         [#xmlElement{content=[#xmlText{value=N}]}] ->
                                             list_to_binary(N)
                                     end,
                              Uri = case xmerl_xpath:string("/author/uri", A) of 
                                        [] -> <<>>;
                                        [#xmlElement{content=[#xmlText{value=U}]}] ->
                                            list_to_binary(U)
                                    end,
                              [{predicate_name, <<"author">>},
                               {object_uri, Uri},
                               {object_title, Name}]
                      end, Authors)
    end.


%% @doc Find the enclosure and store as depiction edge.
%% @spec find_depiction(#xmlElement{}) -> [edge()]
find_depiction(Elem) ->
    case xmerl_xpath:string("/entry/link[@rel=\"enclosure\"]", Elem) of
        [] -> []; % no depiction found
        [Enclosure|_] ->
            [ [{predicate_name, <<"depiction">>},
               {object_uri, xml_attrib(href, Enclosure)},
               {object_title, xml_attrib(title, Enclosure)}
               ] ]
    end.

%% @doc Given an XML element, get the value of an attribute.
%% @spec xml_attrib(atom(), #xmlElement{}) -> binary() | undefined
xml_attrib(Name, #xmlElement{attributes=Attrs}) ->
    case lists:filter(fun(#xmlAttribute{name=Nm}) -> Nm =:= Name end, Attrs) of
        [] -> undefined;
        [#xmlAttribute{value=Value}|_] ->
            list_to_binary(Value)
    end.

%% @doc Given a list of XML test, implode it into one list.
%% @spec collapse_xmltext([#xmlText{}]) -> string()
collapse_xmltext(Content) ->
    lists:flatten([X#xmlText.value || X <- Content]).


%% @doc Given an element, get its XML text. If "strip" attribute is
%% set, text is stripped of (x)html constructs if type attribute is
%% html or xhtml.
get_xmltext(Element=#xmlElement{content=Content}, Strip) ->
    Text = collapse_xmltext(Content),
    Text2 = case Strip of
                false -> Text;
                true ->
                    case xml_attrib(type, Element) of
                        B when B =:= <<"html">> orelse B =:= <<"xhtml">> ->
                            %% Strip tags
                            z_html:strip(Text);
                        B2 when B2 =:= undefined orelse B2 =:= <<"text">> ->
                            %% Do not strip.
                            Text
                    end
            end,
    z_convert:to_binary(Text2).


empty(<<>>) -> true;
empty([])  -> true;
empty(undefined) -> true;
empty(_) -> false.
