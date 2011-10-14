%% @authorTim Benniks <tim@timbenniks.nl>
%% @copyright 2009 Tim Benniks
%% @date 2009-17-08
%% @doc Module implementing the Zotonic web site

%% Copyright 2009 Tim Benniks
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

-module(zotonicwww).
-author("Tim Benniks <tim@timbenniks.nl>").

-mod_title("Zotonic Web Site").
-mod_description("The Zotonic web site.").
-mod_prio(10).
-mod_schema(1).

-export([manage_schema/2]).

-include_lib("zotonic.hrl").

%% @doc Datamodel, installed before this module is started.
manage_schema(install, _) ->
    #datamodel{
           resources =
           [
            {zotonic_news_test1,
             article,
             [{title, <<"Wanna learn more?">>},
              {body, {file, filename:join([z_utils:lib_dir("priv"), "sites", ?MODULE, "demodata", "learnmore.html"])}}]
            },
            {zotonic_news_test0,
             news,
             [{title, <<"Welcome to Zotonic " ?ZOTONIC_VERSION "!">>},
              {summary, <<"Zotonic is the content management system for people that want a fast, extensible, flexible and complete system for dynamic web sites. It is built from the ground up with rich internet applications ánd web publishing in mind.">>},
              {body, {file, filename:join([z_utils:lib_dir(priv), "sites", ?MODULE, "demodata", "welcome.html"])}}
             ]
            },

            {page_blog,
             text,
             [{title, <<"Zotonic Blog">>},
              {short_title, <<"Blog">>},
              {page_path, <<"/blog">>},
              {summary, <<"Read about the latest developments of Zotonic here.">>}]
            }
           ]
          }.
