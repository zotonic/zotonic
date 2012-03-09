%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% Date: 2010-09-20
%% @doc Wordpress WXR import.

%% Copyright 2010,2011 Arjan Scherpenisse
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
-module(import_wordpress).

-export([
         wxr_import/2,
         wxr_import/3,
         wxr_to_datamodel/2,
         test/0
        ]).

-include_lib("xmerl/include/xmerl.hrl").
-include("zotonic.hrl").

-define(RSS_NS, 'http://purl.org/rss/1.0/modules/content/').
-define(WFW_NS, 'http://wellformedweb.org/CommentAPI/').
-define(DC_NS, 'http://purl.org/dc/elements/1.1/').
-define(WP_NS, "http://wordpress.org/export/1.0/").


%% @doc Import a .wxr wordpress file. 
wxr_import(Filename, Context) ->
    wxr_import(Filename, false, Context).


%% @doc Import a .wxr wordpress file. The reset flag controls whether or not previously deleted resources will be recreated.
wxr_import(Filename, Reset, Context) ->
    case Reset of
	true ->
	    z_datamodel:reset_deleted(mod_import_wordpress, Context);
	_ -> ok
    end,
    z_datamodel:manage(mod_import_wordpress, wxr_to_datamodel(Filename, Context), Context).



wxr_to_datamodel(Filename, Context) ->
    {RootElem, _} = xmerl_scan:file(Filename),
    [Channel] = xmerl_xpath:string("/rss/channel", RootElem),

    %% Make sure we support the right version. FIXME - make more flexible.
    [Version] = xmerl_xpath:string("wp:wxr_version", Channel),
    case get_xmltext(Version) of
        <<"1.0">> -> supported;
        <<"1.1">> -> supported;
        V -> throw({error, {unsupported_wxr_version, binary_to_list(V)}})
    end,

    [Link] = xmerl_xpath:string("link", Channel),
    Base = z_convert:to_list(get_xmltext(Link)),

    Data = #datamodel{},

    %% Import the categories as keywords
    Keywords = xmerl_xpath:string("wp:category", Channel),
    {Data1, _, _} = lists:foldl(fun import_wxr_category/2, {Data, Base, Context}, Keywords),

    %% Import all tags 
    Tags = xmerl_xpath:string("//category[@domain=\"tag\"]", Channel),
    {Data2, _, _} = lists:foldl(fun import_wxr_tag/2, {Data1, Base, Context}, Tags),

    %% Import the posts
    Items = xmerl_xpath:string("item", Channel),
    {Data3, _, _} = lists:foldl(fun import_wxr_item/2, {Data2, Base, Context}, Items),

    %% Import the authors
    Authors = xmerl_xpath:string("//dc:creator", Channel),
    {Data4, _, _} = lists:foldl(fun import_wxr_creator/2, {Data3, Base, Context}, Authors),

    %% FIXME in resources; for every category that is of type
    %% 'revision'; only insert the resource with the highest post id
    %% for that parent.

    Data4.


element_content(XPath, Node) ->
    [X] = xmerl_xpath:string(XPath, Node), get_xmltext(X).


import_wxr_item(Item, {Data=#datamodel{resources=R,edges=E}, Base, Context}) ->
    %%?DEBUG(1),
    %%Data2 = Data#datamodel{resources=[1|R]},
    Title = element_content("title", Item),
    Name = "wordpress_" ++ z_convert:to_list(element_content("wp:post_id", Item)),
    Body = element_content("content:encoded", Item),
    Summary = element_content("description", Item),
    IsPublished = map_wp_status(element_content("wp:status", Item)),
    PostParent = z_convert:to_integer(element_content("wp:post_parent", Item)),

    Props =
        [{title, Title},
         {body, wp_embed_tags(Body)},
         {summary, Summary},
         {is_published, IsPublished},
         {custom_slug, true},
         {slug, element_content("wp:post_name", Item)},
         {wp_post_id, element_content("wp:post_id", Item)},
         {wp_post_parent, PostParent}
        ],

    Props1 = case xmerl_xpath:string("wp:attachment_url", Item) of
                 [] -> Props;
                 [U] -> [{media_url, z_convert:to_list(get_xmltext(U))} | Props]
             end,

    Props2 = case z_convert:to_datetime(z_convert:to_list(element_content("wp:post_date", Item))) of
                 {{0,0,0},{0,0,0}} -> Props1;
                 D ->
                     [{publication_start, D}|Props1]
             end,

    Edges = [],

    Edges1 = case xmerl_xpath:string("dc:creator", Item) of
                 [] -> Edges;
                 [C] -> [{Name, author, get_xmltext(C)} | Edges]
             end,

    Edges2 = lists:foldl(fun(Edg, Es) ->
                                 case xml_attrib(nicename, Edg) of
                                     undefined -> Es;
                                     Nm -> [{Name, subject, Nm} | Es]
                                 end
                         end, Edges1, xmerl_xpath:string("category[@domain=\"category\"]", Item)),
    Edges3 = lists:foldl(fun(Edg, Es) ->
                                 case xml_attrib(nicename, Edg) of
                                     undefined -> Es;
                                     Nm -> [{Name, subject, "tag_"++z_convert:to_list(Nm)} | Es]
                                 end
                         end, Edges2, xmerl_xpath:string("category[@domain=\"tag\"]", Item)),

    [Cat0] = xmerl_xpath:string("wp:post_type", Item),
    case map_wp_cat(get_xmltext(Cat0)) of
        undefined ->
            %% Skip unknown categories
            {Data, Base, Context};
        Cat ->
            Edges4 = case Cat =:= media andalso PostParent =/= 0 of 
                         true -> 
                             %% When its a media, look at post parent and insert parent->depiction->media edge.
                             P = lists:flatten(io_lib:format("wordpress_~p", [PostParent])),
                             [ {P, depiction, Name} | Edges3];
                         _ -> 
                             Edges3
                     end,
            Data2 = Data#datamodel{resources=[{Name,Cat,Props2}|R],edges=Edges4++E},
            {Data2, Base, Context}
    end.


import_wxr_creator(Creator, {Data=#datamodel{resources=R}, Base, Context}) ->
    Person = {get_xmltext(Creator),
              person,
              [{title, get_xmltext(Creator)}]},
    Data2 = Data#datamodel{resources=[Person|R]},
    {Data2, Base, Context}.


%% Parse the wp:category tags.
%% <wp:category><wp:category_nicename>art</wp:category_nicename><wp:category_parent></wp:category_parent><wp:cat_name><![CDATA[Art]]></wp:cat_name><wp:category_description><![CDATA[Everything art related. Stuff I like and stuff I hate, and even stuff I don't care about.]]></wp:category_description></wp:category>
import_wxr_category(XmlCat, {Data=#datamodel{resources=R}, Base, Context}) ->
    [Name] = xmerl_xpath:string("wp:category_nicename", XmlCat),
    [Title] = xmerl_xpath:string("wp:cat_name", XmlCat),
    Summary = case xmerl_xpath:string("wp:category_description", XmlCat) of
                  [] -> <<>>;
                  [El] -> get_xmltext(El)
              end,
    Cat = {get_xmltext(Name), keyword, [{title, get_xmltext(Title)}, {summary, Summary}]},
    Data2 = Data#datamodel{resources=[Cat|R]},
    {Data2, Base, Context}.

%% Parse the tags with with posts are tagged.
%% <category domain="tag"><![CDATA[mediamatic workshops toys rfid]]></category>
import_wxr_tag(XmlCat, {Data=#datamodel{resources=R}, Base, Context}) ->
    case xml_attrib(nicename, XmlCat) of
        undefined ->
            {Data, Base, Context};
        Nm ->
            Name = "tag_" ++ z_convert:to_list(Nm),
            Title = get_xmltext(XmlCat),
            Cat = {Name, keyword, [{title, Title}]},
            Data2 = Data#datamodel{resources=[Cat|R]},
            {Data2, Base, Context}
    end.


map_wp_cat(<<"post">>) -> article;
map_wp_cat(<<"page">>) -> text;
map_wp_cat(<<"attachment">>) ->media;
map_wp_cat(_) -> undefined.

map_wp_status(<<"publish">>) -> true;
map_wp_status(<<"inherit">>) -> true;
map_wp_status(_) -> false.

%% @doc Given an element, get its XML text. If "strip" attribute is
%% set, text is stripped of (x)html constructs if type attribute is
%% html or xhtml.
get_xmltext(El) ->
    get_xmltext(El, true).
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


%% @doc Given a list of XML test, implode it into one list.
%% @spec collapse_xmltext([#xmlText{}]) -> string()
collapse_xmltext(Content) ->
    lists:flatten([X#xmlText.value || X <- Content]).

%% @doc Given an XML element, get the value of an attribute.
%% @spec xml_attrib(atom(), #xmlElement{}) -> binary() | undefined
xml_attrib(Name, #xmlElement{attributes=Attrs}) ->
    case lists:filter(fun(#xmlAttribute{name=Nm}) -> Nm =:= Name end, Attrs) of
        [] -> undefined;
        [#xmlAttribute{value=Value}|_] ->
            list_to_binary(Value)
    end.



wp_embed_tags(B) when is_binary(B) ->
    list_to_binary(wp_embed_tags(binary_to_list(B)));

wp_embed_tags(BodyText0) ->
    BodyText = "<p>" ++ re:replace(BodyText0, "\r\n\r\n", "</p><p>", [{return, list}, global]) ++ "</p>",
    Re = "(<a .*?>)?<img.*?class=\"(.*?wp-image-([0-9]+).*?)\".*?/>(</a>)?",
    case re:run(BodyText, Re, [{capture, all}]) of
        nomatch ->
            BodyText;
        {match, [{Start, Len}, {-1,0}, {CS, CL}, {IdStart,IdL}]} ->
            Class = string:substr(BodyText, CS+1,CL),
            Opts = class_to_embed_opts(Class, false),
            string:substr(BodyText, 1, Start) 
                ++ "<!-- z-media wordpress_" ++ string:substr(BodyText, IdStart+1, IdL) ++ " " ++ Opts ++ " -->"
                ++ wp_embed_tags(string:substr(BodyText, Start+Len+1));
        {match, [{Start, Len}, {_AS,_AL}, {CS, CL}, {IdStart,IdL}, {_,4}]} ->
            Class = string:substr(BodyText, CS+1,CL),
            Opts = class_to_embed_opts(Class, true),
            string:substr(BodyText, 1, Start) 
                ++ "<!-- z-media wordpress_" ++ string:substr(BodyText, IdStart+1, IdL) ++ " " ++ Opts ++ " -->"
                ++ wp_embed_tags(string:substr(BodyText, Start+Len+1))
%%            ?DEBUG(F)
%%            {match, [{Start, Len}, {X,Y}, {CS, CL}, {IdStart,IdL}]} ->

    end.


class_to_embed_opts(Class, Link) ->
    Ln = case Link of 
             true -> "\"link\":true, ";
             false -> []
         end,
    L = lists:flatten([map_class_attr(C) || C <- string:tokens(Class, " ")])++Ln,
    "{" ++ string:substr(L, 1, length(L)-2) ++ "}".

map_class_attr("alignnone") -> "\"align\":\"block\", ";
map_class_attr("alignright") -> "\"align\":\"right\", ";
map_class_attr("alignleft") -> "\"align\":\"left\", ";
map_class_attr("size-full") -> "\"size\":\"large\", ";
map_class_attr("size-medium") -> "\"size\":\"middle\", ";
map_class_attr("size-thumbnail") -> "\"size\":\"small\", ";
map_class_attr(_) -> [].


test() ->
    "<!-- z-media wordpress_29 {align:\"right\", size:\"large\"} -->" = wp_embed_tags("<img class=\"alignright size-full wp-image-29\" style=\"border: 0pt none; margin-left: 10px;\" title=\"Announcement Retroweek\" src=\"http://idum5.net/blog/wp-content/uploads/poster-klein.jpg\" alt=\"Announcement Retroweek\" width=\"217\" height=\"307\" />"),

    "foobar <!-- z-media wordpress_29 {align:\"left\", size:\"middle\"} -->baz" = wp_embed_tags("foobar <img class=\"alignleft size-medium wp-image-29\" style=\"border: 0pt none; margin-left: 10px;\" title=\"Announcement Retroweek\" src=\"http://idum5.net/blog/wp-content/uploads/poster-klein.jpg\" alt=\"Announcement Retroweek\" width=\"217\" height=\"307\" />baz"),

    "foo<!-- z-media wordpress_29 {align:\"right\", size:\"large\"} -->bar<!-- z-media wordpress_29 {align:\"right\", size:\"large\"} -->baz" = 
        wp_embed_tags("foo<img class=\"alignright size-full wp-image-29\" style=\"border: 0pt none; margin-left: 10px;\" title=\"Announcement Retroweek\" src=\"http://idum5.net/blog/wp-content/uploads/poster-klein.jpg\" alt=\"Announcement Retroweek\" width=\"217\" height=\"307\" />bar<img class=\"alignright size-full wp-image-29\" style=\"border: 0pt none; margin-left: 10px;\" title=\"Announcement Retroweek\" src=\"http://idum5.net/blog/wp-content/uploads/poster-klein.jpg\" alt=\"Announcement Retroweek\" width=\"217\" height=\"307\" />baz"),


    "<!-- z-media wordpress_29 {align:\"right\", size:\"large\", link:true} -->" = wp_embed_tags("<a href=\"http://idum5.net/blog/wp-content/uploads/screenshot4.png\"><img class=\"alignright size-full wp-image-29\" style=\"border: 0pt none; margin-left: 10px;\" title=\"Announcement Retroweek\" src=\"http://idum5.net/blog/wp-content/uploads/poster-klein.jpg\" alt=\"Announcement Retroweek\" width=\"217\" height=\"307\" /></a>"),

    ok.

    
