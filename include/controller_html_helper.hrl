%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Empty HTML controller, defining the basic callbacks needed for a html page.  All is needed is the 'html' function.
%% Make a new controller (controller_plop.erl) by:
%%
%% -module(controller_plop).
%% -author("Your Name <me@example.com>").
%% -include_lib("controller_html_helper.hrl").
%% 
%% html(Context) ->
%%    Html = z_template:render("plop.tpl", Context),
%%    z_context:output(Html, Context).

%% Copyright 2009 Marc Worrell
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

-export([to_html/1, charsets_provided/1]).

-include_lib("include/zotonic.hrl").

charsets_provided(Context) ->
    {[<<"utf-8">>], Context}.

to_html(Context) ->
    Context2 = z_context:ensure_all(Context),
    z_context:lager_md(Context2),
    html(Context2).
