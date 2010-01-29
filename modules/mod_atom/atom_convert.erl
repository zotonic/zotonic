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

%% @doc Export a resource to Atom XML.
%% @spec resource(rsc_export()) -> string().
resource_to_atom(RscExport) ->
    Rsc = proplists:get_value(rsc, RscExport),

    Content0 = [
                {id, [binary_to_list(proplists:get_value(resource_uri, RscExport))]},
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



%% @doc Export a resource to Atom XML.
%% @spec resource(string()) -> rsc_export().
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
                 [#xmlElement{content=Title}] ->
                     [{title, list_to_binary(collapse_xmltext(Title))}]
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
               [#xmlElement{content=Summary}] ->
                   RscProps3 ++ [{summary, list_to_binary(collapse_xmltext(Summary))}]
           end,

    RscProps5 = case xmerl_xpath:string("/entry/content", RootElem) of
               [] -> RscProps4;
               [#xmlElement{content=Body}] ->
                   RscProps4 ++ [{body, list_to_binary(collapse_xmltext(Body))}]
           end,

    [{resource_uri, RscUri},
     {rsc, RscProps5}
    ].


collapse_xmltext(Content) ->
    lists:flatten([X#xmlText.value || X <- Content]).


empty(<<>>) -> true;
empty([])  -> true;
empty(undefined) -> true;
empty(_) -> false.
