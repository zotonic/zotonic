%% @author William Fank Thomé <williamthome@hotmail.com>
%% @copyright 2022-2023 William Fank Thomé
%% @doc Open a site in Chrome or Chromium.
%% @end

%% Copyright 2009-2023 William Fank Thomé
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

-module(z_exec_browser).

-export([
    open/1,
    chrome/1,
    chrome/2,
    chrome/3,
    chromium/1,
    chromium/2,
    chromium/3,
    exec_browser/4,
    exec_browser/5
    ]).

-include("../../include/zotonic.hrl").


%% @doc Open a site on macOS in the default browser, on other platforms open
%% in Chrome.
-spec open(SiteOrContext) -> RetType
    when
        SiteOrContext :: atom() | z:context(),
        RetType       :: ok | {error, term()}.
open(#context{} = Context) ->
    case os:type() of
        {unix, darwin} ->
            SiteUrl = z_context:abs_url(<<"/">>, Context),
            Cmd = "open " ++ z_utils:os_filename(SiteUrl),
            do_exec_browser(Cmd);
        _ ->
            chrome(Context)
    end;
open(Site) when is_atom(Site) ->
    case z_sites_manager:get_site_status(Site) of
        {ok, running} ->
            open(z_context:new(Site));
        {ok, Status} ->
            {error, Status};
        {error, _} = Error ->
            Error
    end.

%% @doc Runs Chrome opening it in the site URL.
%% Ignore certificate errors and defines the site as secure, helpful to run Web Workers.
%% For extra args @see https://peter.sh/experiments/chromium-command-line-switches/
%% Common args:
%%   --incognito            Launches Chrome directly in Incognito private browsing mode
%%   --purge-memory-button  Add purge memory button to Chrome
%%   --multi-profiles       Enable multiple profiles in Chrome
%% e.g.
%% ``` z_exec_browser:chrome(foo, ["--incognito", "--start-maximized"]). '''
chrome(SiteOrContext) ->
    chrome(SiteOrContext, []).

chrome(SiteOrContext, ExtraArgs) ->
    chrome(SiteOrContext, ExtraArgs, #{}).

chrome(SiteOrContext, ExtraArgs, Options) ->
    exec_browser(chrome, SiteOrContext, ExtraArgs, Options).

%% @doc Runs Chromium opening it in the site URL.
%% Ignore certificate errors and defines the site as secure, helpful to run Web Workers.
%% For extra args @see https://peter.sh/experiments/chromium-command-line-switches/
%% Common args:
%%   --incognito            Launches Chromium directly in Incognito private browsing mode
%%   --purge-memory-button  Add purge memory button to Chromium
%%   --multi-profiles       Enable multiple profiles in Chromium
%% e.g.
%% ``` z_exec_browser:chromium(foo, ["--incognito", "--start-maximized"]). '''
chromium(SiteOrContext) ->
    chromium(SiteOrContext, []).

chromium(SiteOrContext, ExtraArgs) ->
    chromium(SiteOrContext, ExtraArgs, #{}).

chromium(SiteOrContext, ExtraArgs, Options) ->
    exec_browser(chromium, SiteOrContext, ExtraArgs, Options).

%% @doc Opens the site URL as secure in a browser
%% Currently supported:
%%   * Linux  [Chrome, Chromium]
%%   * macOS  [Chrome, Chromium]
%% @todo: support more OS and maybe other browsers
-spec exec_browser(Browser, SiteOrContext, ExtraArgs, Options) -> RetType
    when
        Browser       :: atom(),
        SiteOrContext :: atom() | z:context(),
        ExtraArgs     :: [string()],
        Options       :: map(),
        RetType       :: ok | {error, term()}.
exec_browser(Browser, #context{} = Context, ExtraArgs, Options) ->
    OS = os:type(),
    SiteUrl = z_context:abs_url(<<"/">>, Context),
    exec_browser(Browser, OS, SiteUrl, ExtraArgs, Options);
exec_browser(Browser, Site, ExtraArgs, Options) when is_atom(Site) ->
    case z_sites_manager:get_site_status(Site) of
        {ok, running} ->
            exec_browser(Browser, z_context:new(Site), ExtraArgs, Options);
        {ok, Status} ->
            {error, Status};
        {error, _} = Error ->
            Error
    end.

exec_browser(chrome, {unix, linux}, SiteUrl, ExtraArgs, Options) ->
    case os:find_executable("google-chrome") of
        false ->
            {error, "Chrome executable not found."};
        Executable ->
            exec_chrome(Executable, SiteUrl, ExtraArgs, Options)
    end;
exec_browser(chrome, {unix, darwin}, SiteUrl, ExtraArgs, Options) ->
    Executable = "/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome",
    exec_chrome(Executable, SiteUrl, ExtraArgs, Options);
exec_browser(chromium, {unix, linux}, SiteUrl, ExtraArgs, Options) ->
    case os:find_executable("chromium-browser") of
        false ->
            {error, "Chromium executable not found."};
        Executable ->
            exec_chrome(Executable, SiteUrl, ExtraArgs, Options)
    end;
exec_browser(chromium, {unix, darwin}, SiteUrl, ExtraArgs, Options) ->
    Executable = "/Applications/Chromium.app/Contents/MacOS/Chromium",
    exec_chrome(Executable, SiteUrl, ExtraArgs, Options);
exec_browser(_Browser, _OS, _SiteUrl, _ExtraArgs, _Options) ->
    {error, "Browser or operating system not supported."}.


exec_chrome(Executable, SiteUrl, ExtraArgs, #{secure := true} = Options) ->
    TmpPath = filename:join([ z_tempfile:temppath(), "zotonic-chrome" ]),
    Args = lists:join(" ", [
        "--user-data-dir=" ++ TmpPath,
        "--ignore-certificate-errors",
        "--unsafely-treat-insecure-origin-as-secure=" ++ SiteUrl
        | ExtraArgs
    ]),
    do_exec_chrome(Executable, SiteUrl, Args, Options);
exec_chrome(Executable, SiteUrl, ExtraArgs, Options) ->
    do_exec_chrome(Executable, SiteUrl, ExtraArgs, Options).

do_exec_chrome(Executable, SiteUrl, Args, _Options) ->
    Command = io_lib:format("~s ~s ~s", [
        z_filelib:os_escape(Executable), Args, z_filelib:os_filename(SiteUrl)
    ]),
    io:format(
        "Trying to execute the commmand:\n$ ~s\n",
        [unicode:characters_to_list(Command)]
    ),
    do_exec_browser(Command).

do_exec_browser(Command) ->
    case catch open_port({spawn, Command}, [in, hide]) of
        Port when is_port(Port) ->
            ok;
        Error ->
            {error, Error}
    end.
