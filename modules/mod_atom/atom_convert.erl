%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% @date 2009-10-02
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


resource_to_atom(X) ->
    Rsc = proplists:get_value(rsc, X),

    Content0 = [
                {id, [binary_to_list(proplists:get_value(resource_uri, Rsc))]},
                {title, [{type, "html"}], [binary_to_list(proplists:get_value(title, Rsc))]},
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
                       Content1 ++ [{summary, [{type, "html"}], [binary_to_list(Summary)]}]
               end,

    RootElem = #xmlElement{name=entry, 
                           namespace=#xmlNamespace{default=?ATOM_NS},
                           attributes=[#xmlAttribute{name=xmlns, value=?ATOM_NS}],
                           content=Content2},
    lists:flatten(xmerl:export_simple([RootElem], xmerl_xml)).


-define(simpleContent(Expr, Elem), collapse_xmltext((hd(xmerl_xpath:string(Expr, Elem)))#xmlElement.content) ).

atom_to_resource(Xml) ->
    {RootElem,_} = xmerl_scan:string(Xml),

    %% Atom required elements
    Props0 = case xmerl_xpath:string("/entry/id", RootElem) of
                 [] -> [];
                 [#xmlElement{content=Uri}] ->
                     [{resource_uri, list_to_binary(collapse_xmltext(Uri))}]
             end,

    Props1 = case xmerl_xpath:string("/entry/title", RootElem) of
                 [] -> Props0 ++ [{title, <<>>}];
                 [#xmlElement{content=Title}] ->
                     Props0 ++ [{title, list_to_binary(collapse_xmltext(Title))}]
             end,

    Props2 = case xmerl_xpath:string("/entry/updated", RootElem) of
                 [] -> Props1;
                 [#xmlElement{content=Updated}] ->
                     Props1 ++ [{modified, z_convert:to_datetime(collapse_xmltext(Updated))}]
             end,

    Props3 = case xmerl_xpath:string("/entry/published", RootElem) of
                 [] -> Props2;
                 [#xmlElement{content=Published}] ->
                     Props2 ++ [{publication_start, z_convert:to_datetime(collapse_xmltext(Published))}]
             end,

    Props4 = case xmerl_xpath:string("/entry/summary", RootElem) of
               [] -> Props3;
               [#xmlElement{content=Summary}] ->
                   Props3 ++ [{summary, list_to_binary(collapse_xmltext(Summary))}]
           end,

    Props5 = case xmerl_xpath:string("/entry/content", RootElem) of
               [] -> Props4;
               [#xmlElement{content=Body}] ->
                   Props4 ++ [{body, list_to_binary(collapse_xmltext(Body))}]
           end,

    [{rsc, Props5}].


collapse_xmltext(Content) ->
    lists:flatten([X#xmlText.value || X <- Content]).


empty(<<>>) -> true;
empty([])  -> true;
empty(undefined) -> true;
empty(_) -> false.
