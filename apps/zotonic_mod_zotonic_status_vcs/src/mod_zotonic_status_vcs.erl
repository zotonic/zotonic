%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @date 2010-09-13
%% @doc Mercurial support for zotonic sites

%% Copyright 2010 Marc Worrell
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

-module(mod_zotonic_status_vcs).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Mercurial support").
-mod_description("Allows to update and rebuild sites and zotonic installs from git or mercurial.").
-mod_prio(500).

-export([
    observe_zotonic_status_init/3,
    event/2
]).

-export([notice/3, show_notice/3]).

-include_lib("zotonic_core/include/zotonic.hrl").


-spec event(#postback{}, z:context()) -> z:context().
event(#postback{message={vcs_up, Args}}, Context) ->
    true = z_auth:is_auth(Context),
    case proplists:get_value(zotonic, Args) of
        true ->
            case has_vcs() of
                undefined ->
                    notice('Zotonic', <<"Zotonic hasn’t been checked out using version control."/utf8>>, Context);
                {hg, Path} ->
                    show_notice('Zotonic', <<"Fetching updates…"/utf8>>, Context),
                    Command = lists:flatten(["(cd \"", Path, "\"; hg pull -u)"]),
                    notice('Zotonic', os:cmd(Command), Context);
                {git, Path} ->
                    show_notice('Zotonic', <<"Fetching updates…"/utf8>>, Context),
                    Command = lists:flatten(["(cd \"", Path, "\"; git pull)"]),
                    notice('Zotonic', os:cmd(Command), Context)
            end;
        undefined ->
            Site = proplists:get_value(site, Args),
            case has_vcs(Site) of
                {hg, Path} ->
                    show_notice(Site, <<"Fetching updates…"/utf8>>, Context),
                    Command = lists:flatten(["(cd \"", Path, "\"; hg pull -u)"]),
                    notice(Site, os:cmd(Command), Context);
                {git, Path} ->
                    show_notice(Site, <<"Fetching updates…"/utf8>>, Context),
                    Command = lists:flatten(["(cd \"", Path, "\"; git pull)"]),
                    show_notice(Site, os:cmd(Command), Context);
                undefined ->
                    notice(Site, <<"Unknown site or nor mercurial/git folder present.">>, Context)
            end
    end;
event(#postback{message=make}, Context) ->
    true = z_auth:is_auth(Context),
    spawn(fun() ->
                z:m(),
                show_notice('Zotonic', <<"Finished rebuilding Zotonic.">>, Context)
              end),
    notice('Zotonic', <<"Building Zotonic in the background…"/utf8>>, Context).


% @doc Show a notice on the current webpage.
-spec show_notice(atom(), string(), z:context()) -> z:context().
show_notice(SiteName, Text, Context) ->
    z_session_page:add_script(notice(SiteName, Text, Context)).

% @doc Render a notice.
-spec notice(atom(), string(), z:context()) -> z:context().
notice(SiteName, Text, Context) ->
    Context1 = z_render:appear_top(
                        "notices",
                        #render{template="_notice.tpl", vars=[{site,SiteName},{notice,Text}]},
                        Context),
    z_render:wire({fade_out, [{selector, "#notices > div:gt(0)"}, {speed, 2000}]}, Context1).


%% @doc Check which sites have a .hgrc directory. Add this info to the template vars.
observe_zotonic_status_init(zotonic_status_init, Vars, _Context) ->
    case proplists:get_value(has_user, Vars) of
        undefined -> Vars;
        N when is_integer(N) ->
            Sites = [ SiteName || {SiteName,_Props} <- proplists:get_value(configs, Vars) ],
            Vcs = [ {SiteName, has_vcs(SiteName)} || SiteName <- Sites ],
            [{vcs, Vcs}, {vcs_zotonic, has_vcs()} | Vars]
    end.

% @doc Check if the site directory has a mercurial .hg subdirectory
has_vcs(Site) ->
    has_vcs_dir(z_path:site_dir(Site)).

%% @doc Check if zotonic itself has a .hg directory
has_vcs() ->
    has_vcs_dir(z_utils:lib_dir()).

has_vcs_dir(Dir) ->
    HgDir = filename:join([Dir, ".hg"]),
    case filelib:is_dir(HgDir)  of
        true -> {hg, Dir};
        false ->
            GitDir = filename:join([Dir, ".git"]),
            case filelib:is_dir(GitDir) of
                true -> {git, Dir};
                false -> undefined
            end
    end.
