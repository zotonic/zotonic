%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Remove private media artifacts even when their owning worker is killed.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_media_runner_temp).
-export([guard/1, release/1]).

-spec guard(file:filename_all()) -> pid().
guard(Path) ->
    Parent = self(),
    Tag = make_ref(),
    {Pid, Monitor} = spawn_monitor(fun() ->
        Ref = monitor(process, Parent),
        Parent ! {Tag, ready},
        receive
            {release, From, ReleaseTag} -> file:delete(Path), From ! {ReleaseTag, released};
            {'DOWN', Ref, process, Parent, _} -> file:delete(Path)
        end
    end),
    receive
        {Tag, ready} -> demonitor(Monitor, [flush]), Pid;
        {'DOWN', Monitor, process, Pid, Reason} -> error({media_temp_guard, Reason})
    end.

-spec release(pid()) -> ok.
release(Pid) ->
    Ref = monitor(process, Pid),
    Pid ! {release, self(), Ref},
    receive
        {Ref, released} -> demonitor(Ref, [flush]), ok;
        {'DOWN', Ref, process, Pid, _} -> ok
    end.

