%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% @date 2009-12-12
%% @doc Module implementing a basic blog.

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

-module(default).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").
-behaviour(gen_server).

-mod_title("Blog").
-mod_description("A simple weblog, used as an example of how to create a Zotonic site.").
-mod_prio(10).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
]).

-include_lib("zotonic.hrl").

-record(state, {context}).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    z_datamodel:manage(?MODULE, datamodel(), Context),
    {ok, #state{context=z_context:new(Context)}}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================


datamodel() ->
    Now = erlang:localtime(),
    [
     {resources,
      [

       %% MENU ENTRIES

       {page_home,
        text,
        [{title, <<"Welcome to your blog!">>}]
       },

       {page_about,
        text,
        [{title, <<"About this blog">>},
         {summary, <<"This is your blog. It would be wise to type some text here on what you will be writing about. Ofcourse, this page is just a demo page and can be deleted just as well.">>}]
       },


       %% BLOG ENTRIES

       {blog_article_welcome,
        article,
        [{title, <<"Welcome to Zotonic " ?ZOTONIC_VERSION "!">>},
         {publication_start, Now},
         {summary, <<"Zotonic is the content management system for people that want a fast, extensible, flexible and complete system for dynamic web sites. It is built from the ground up with rich internet applications ánd web publishing in mind.">>},
         {body, {file, filename:join([z_utils:lib_dir(priv), "sites", ?MODULE, "demodata", "welcome.html"])}}
        ]
       },
       {blog_article_learnmore,
        article,
        [{title, <<"Want to learn more?">>},
         {publication_start, z_datetime:prev_day(Now)},
         {summary, <<"This blog website you're looking demonstrates only a small part of what you can do with a Zotonic site. For instance, did you know that sending mass-mailings is a builtin module? That it does OAuth out of the box? That Zotonic sites are SEO optimized by default?">>},
         {body, {file, filename:join([z_utils:lib_dir(priv), "sites", ?MODULE, "demodata", "learnmore.html"])}}]
       },
       {blog_article_demo,
        article,
        [{title, <<"Zotonic's Typography">>},
         {publication_start, z_datetime:prev_month(Now)},
         {summary, <<"This article demonstrates the typographic features that Zotonic has. It shows creating ordered and unordered lists, blockquotes, and different methods of embedding media, even even showing an embedded Youtube video.">>},
         {body, {file, filename:join([z_utils:lib_dir(priv), "sites", ?MODULE, "demodata", "demo.html"])}}
        ]
       }

      ]
     },

     {menu,
      [page_home, page_about]
     },

     {edges,
      [
       {blog_article_learnmore, author, administrator},
       {blog_article_welcome, author, administrator},
       {blog_article_demo, author, administrator}
      ]
     }
    ].
