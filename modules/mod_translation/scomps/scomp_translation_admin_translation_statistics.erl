%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse

%% Copyright 2010 Arjan Scherpenisse
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

-module(scomp_translation_admin_translation_statistics).
-behaviour(gen_scomp).

-export([vary/2, render/3]).

-include("zotonic.hrl").


vary(_Params, _Context) -> nocache.
render(Params, _Vars, Context) ->
    {module, Module} = proplists:lookup(module, Params),
    {lang, Lang} = proplists:lookup(lang, Params),

    %% Lookup the path to the module
    AllDirs = z_module_manager:active_dir(Context),
    BasePath = proplists:get_value(z_convert:to_atom(Module), AllDirs),

    %% First check if module has a translations template file. If not, return "n/a"
    PotFile = filename:join([BasePath, "translations", "template", z_convert:to_list(Module) ++ ".pot"]),
    case filelib:is_regular(PotFile) of
        true ->
            TotalLines = length(z_gettext:parse_po(PotFile)),
            PoFile = filename:join([BasePath, "translations", z_convert:to_list(Lang) ++ ".po"]),
            case filelib:is_regular(PoFile) of
                true ->
                    C = count_translated(PoFile),
                    Cls = erlang:min(100, trunc(((C/TotalLines) * 100)/20)*20),
                    {ok, ["<td class=\"perc perc-", z_convert:to_list(Cls), "\"><span>", z_convert:to_list(C), " / ", z_convert:to_list(TotalLines), "</span></td>"]};
                false ->
                    {ok, ["<td class=\"perc perc-0\"><span>0 / ", z_convert:to_list(TotalLines), "</span></td>"]}
            end;
        false ->
            {ok, <<"<td class=\"zp-10 perc\"><span>N/A</span></td>">>}
    end.


count_translated(File) ->
    lists:foldl(fun(Line, Total) ->
                        is_translated(Line) + Total
                end,
                0,
                z_gettext:parse_po(File)).


is_translated({header, _}) -> 1;
is_translated({_, header}) -> 0;
is_translated({X, X}) -> 0;
is_translated({_, []}) -> 0;
is_translated({_, _}) -> 1.
