%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Asynchronous single-flight persistence of analytics batches.
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

-module(z_analytics_writer).
-behaviour(gen_server).
-moduledoc("Single-flight analytics database writer; ingestion never waits for SQL.").

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Site) ->
    gen_server:start_link(
        {local,z_utils:name_for_site(analytics_writer,Site)},
        ?MODULE,
        Site,
        []).

init(Site) ->
    {ok,Site}.

handle_call(_,_,Site) ->
    {reply,{error,unknown_call},Site}.

handle_cast({flush,Pid,Epoch,Seq,Batch,Cs},Site) ->
    Context = z_context:new(Site),
    Result = try
            m_analytics:store_flush(Epoch,Seq,Batch,Cs,Context)
        catch
            Class:Reason -> {error,{Class,Reason}}
        end,
    Pid ! {flushed,Seq,Result},
    {noreply,Site};
handle_cast(_,Site) ->
    {noreply,Site}.

handle_info(_,Site) ->
    {noreply,Site}.
