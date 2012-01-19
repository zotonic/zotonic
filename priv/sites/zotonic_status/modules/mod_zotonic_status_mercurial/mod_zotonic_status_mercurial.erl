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

-module(mod_zotonic_status_mercurial).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Mercurial support").
-mod_description("Allows to update and rebuild sites and zotonic installs from mercurial.").
-mod_prio(500).

-export([
    observe_zotonic_status_init/3,
    event/2
]).

-include("zotonic.hrl").


event(#postback{message={hgup, Args}}, Context) ->
    true = z_auth:is_auth(Context),
    case proplists:get_value(zotonic, Args) of
        true ->
            case has_hg() of
                undefined -> 
                    notice('Zotonic', "Zotonic hasn’t been checked out using Mercurial.", Context);
                Path ->
                    z_session_page:add_script(notice('Zotonic', "Fetching updates…", Context)),
                    Command = lists:flatten(["(cd \"", Path, "\"; hg pull -u)"]),
                    z_session_page:add_script(notice('Zotonic', os:cmd(Command), Context)),
                    Context
            end;
        undefined -> 
            Site = proplists:get_value(site, Args),
            Path = site_path(Site),
            case filelib:is_dir(Path) of
                true ->
                    z_session_page:add_script(notice(Site, "Fetching updates…", Context)),
                    Command = lists:flatten(["(cd \"", Path, "\"; hg pull -u)"]),
                    z_session_page:add_script(notice(Site, os:cmd(Command), Context)),
                    Context;
                false ->
                    notice(Site, "Unknown site or nor mercurial folder present.", Context)
            end
    end;
event(#postback{message=make}, Context) ->
    true = z_auth:is_auth(Context),
    spawn(fun() -> 
            z:m(),
            z_session_page:add_script(notice('Zotonic', "Finished rebuilding Zotonic.", Context))
          end),
    notice('Zotonic', "Building Zotonic in the background…", Context).


notice(SiteName, Text, Context) ->
    Context1 = z_render:appear_top(
                        "notices", 
                        #render{template="_notice.tpl", vars=[{site,SiteName},{notice,Text}]}, 
                        Context),
    z_render:wire({fade_out, [{selector, "#notices > p:gt(0)"}, {speed, 1000}]}, Context1).


%% @doc Check which sites have a .hgrc directory. Add this info to the template vars.
observe_zotonic_status_init(zotonic_status_init, Vars, _Context) ->
    case proplists:get_value(has_user, Vars) of
        undefined -> Vars;
        N when is_integer(N) ->
            Sites = [ SiteName || {SiteName,_Props} <- proplists:get_value(configs, Vars) ],
            Hg = [ {SiteName, has_hg(SiteName)} || SiteName <- Sites ],
            [{hg, Hg}, {hg_zotonic, has_hg()} | Vars]
    end.
    
% @doc Check if the site directory has a mercurial .hg subdirectory
has_hg(Site) ->
    HgDir = filename:join([site_path(Site), ".hg"]),
    case filelib:is_dir(HgDir) of
        true -> HgDir;
        false -> undefined
    end.

%% @doc Check if zotonic itself has a .hg directory
has_hg() ->
    HgDir = filename:join([z_utils:lib_dir(), ".hg"]),
    case filelib:is_dir(HgDir) of
        true -> HgDir;
        false -> undefined
    end.


site_path(Site) ->
    filename:join([z_utils:lib_dir(priv), "sites", Site]).

