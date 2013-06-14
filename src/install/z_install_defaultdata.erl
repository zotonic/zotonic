%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse
%%
%% @doc Installs default data in a Zotonic site.

%% Copyright 2011 Arjan Scherpenisse
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

-module(z_install_defaultdata).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include("zotonic.hrl").

%% interface functions
-export([
    install/2,
    default_menu/1
]).


install(basesite, Context) ->
    Datamodel = #datamodel{
         resources =
             [
              {page_home,
               text,
               [{title, <<"Home">>},
                {summary, <<"Welcome to your new site!">>},
                {page_path, <<"/">>}]
              }
             ]
        },
    ?DEBUG("Installing basesite data"),
    z_datamodel:manage(?MODULE, Datamodel, Context);

install(blog, Context) ->
    Now = {{2012,12,14},{9,12,0}},
    Datamodel = 
        #datamodel{
      resources =
      [
       
       %% MENU ENTRIES
       
       {page_home,
        text,
        [{title, <<"Home">>},
         {summary, <<"Welcome to your blog!">>},
         {page_path, <<"/">>}]
       },

       {page_about,
        text,
        [{title, <<"About this blog">>},
         {summary, <<"This is your blog!! It would be wise to type some text here on what you will be writing about. Of course, this page is just a demo page and can be deleted just as well.">>}]
       },

       {page_contact,
        text,
        [{title, <<"Contact">>},
         {summary, <<"Get in contact with us! Use the form below to send this site's administrator some feedback on how you perceive this site.">>},
         {page_path, <<"/contact">>}]
       },

       %% BLOG ENTRIES

       {blog_article_welcome,
        article,
        [{title, <<"Welcome to Zotonic!">>},
         {publication_start, Now},
         {summary, <<"Zotonic is the content management system for people that want a fast, extensible, flexible and complete system for dynamic web sites. It is built from the ground up with rich internet applications and web publishing in mind.">>},
         {body, {file, datafile(blog, "welcome.html")}}
        ]
       },
       {blog_article_learnmore,
        article,
        [{title, <<"Want to learn more?">>},
         {publication_start, z_datetime:prev_day(Now)},
         {summary, <<"This blog website you're looking demonstrates only a small part of what you can do with a Zotonic site. For instance, did you know that sending mass-mailings is a builtin module? That it does OAuth out of the box? That Zotonic sites are SEO optimized by default?">>},
         {body, {file, datafile(blog, "learnmore.html")}}
        ]
       },
       {blog_article_demo,
        article,
        [{title, <<"Zotonic's Typography">>},
         {publication_start, z_datetime:prev_month(Now)},
         {summary, <<"This article demonstrates the typographic features that Zotonic has. It shows creating ordered and unordered lists, blockquotes, and different methods of embedding media, even even showing an embedded video from Vimeo.com.">>},
         {body, {file, datafile(blog, "demo.html")}}
        ]
       },

       %% KEYWORDS

       {kw_announcement,
        keyword,
        [{title, <<"Announcement">>}]
       },
       {kw_technical,
        keyword,
        [{title, <<"Technical">>}]
       },
       {kw_support,
        keyword,
        [{title, <<"Support">>}]
       }
      ],

      media =
      [
       {media_learning,
        datafile(blog, "learning.jpg"),
        [{title, <<"A bunch of computer books">>},
         {summary, <<"Taken by Sibi from Flickr, licensed Attribution-Noncommercial-No Derivative Works 2.0.">>}]
       },
       {media_welcome,
        datafile(blog, "welcome.jpg"),
        [{title, <<"Rocky sunrise">>},
         {summary, <<"Taken by Grant MacDonald from Flickr, CC licensed Attribution-Noncommercial 2.0.">>}]
       },
       {media_video,
        [{title, <<"Zotonic introduction video">>},
         {oembed_url, <<"http://vimeo.com/7630916">>}]
       }
      ],

      edges = 
      [
       {blog_article_learnmore, author, administrator},
       {blog_article_welcome, author, administrator},
       {blog_article_demo, author, administrator},
       
       {blog_article_learnmore, subject, kw_support},
       {blog_article_demo, subject, kw_technical},
       {blog_article_welcome, subject, kw_support},
       {blog_article_welcome, subject, kw_announcement},
       
       {blog_article_welcome, depiction, media_welcome},
       {blog_article_learnmore, depiction, media_learning},
       {blog_article_demo, depiction, media_welcome}
       
      ]
     },

    ?DEBUG("Installing blog data"),
    z_datamodel:manage(?MODULE, Datamodel, Context);


install(_, _) ->
    %% no/unknown skeleton = no default data
    ok.


%% @doc Retrieve the default menu structure for a given site. Used by mod_menu to create the menu.
%% The menu can be defined in the site config file as a list of menu items under the key <tt>install_menu</tt>.
%% If that is not defined, a default menu for the skeleton is used, if any.
-spec default_menu(#context{}) -> MenuItems | undefined when
      MenuItems :: [MenuItem],
      MenuItem :: {PageName, MenuItems},
      PageName :: atom().
default_menu(Context) ->
    case m_site:get(install_menu, Context) of
        Menu when is_list(Menu) -> Menu;
        _ -> default_skeleton_menu(m_site:get(skeleton, Context))
    end.

default_skeleton_menu(blog) ->
    [{page_home, []}, {page_about, []}, {page_contact, []}];
default_skeleton_menu(_) ->
    %% no/unknown skeleton = no default menu
    undefined.

%% @doc Helper function for getting an absolute path to a data file
%%      that is part of the default data for a site skeleton.
datafile(Skeleton, Filename) ->
    filename:join([z_utils:lib_dir(priv), "install", Skeleton, Filename]).
