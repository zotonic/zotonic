%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Periodically cleanup leftover tempfiles in the tempdir

%% Copyright 2015 Marc Worrell
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

-module(z_tempfile_cleanup).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    start/0,
    cleanup/0
]).

%% Run every 4 hours
-define(INTERVAL, 1000*3600*4).

%% Tempfiles must at least be one day old, before deletion
-define(AGE, 3600*24).

start() ->
    timer:apply_interval(?INTERVAL, ?MODULE, cleanup, []).

cleanup() ->
    TmpDir = z_tempfile:temppath(),
    {ok, Fs} = file:list_dir(TmpDir),
    Fs1 = [ filename:join(TmpDir, F) || F <- Fs ],
    Fs2 = lists:filter(fun z_tempfile:is_tempfile/1, Fs1),
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    lists:foreach(fun(F) ->
                    maybe_delete(F, NowSecs - ?AGE)
                  end,
                  Fs2).

maybe_delete(File, Youngest) ->
    case z_email_server:is_tempfile(File) of
        true ->
            case z_email_server:is_tempfile_deletable(File) of
                true -> file:delete(File);
                false -> ok
            end;
        false ->
            case is_deletable(File, Youngest) of
                true -> file:delete(File);
                false -> ok
            end
    end.

is_deletable(File, Youngest) ->
    case filelib:last_modified(File) of
        0 ->
            false;
        Modified when is_tuple(Modified) ->
            calendar:datetime_to_gregorian_seconds(Modified) < Youngest
    end.
