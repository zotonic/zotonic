%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Build bounded time-series chart data from analytics aggregates.
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

-module(z_analytics_chart).
-moduledoc("Build total and breakdown series on a shared time axis, preserving missing intervals.").
-export([series/8]).
-include_lib("zotonic_core/include/zotonic.hrl").

series(Rows,Start,End,R,N,Groups,Col,Context) ->
    Index=maps:from_list([{maps:get(start,X),X}||X<-Rows]),
    Step=max(1,(N+239) div 240),
    Points=[point(T,min(End,T+R*Step),R,Index)||T<-lists:seq(Start,End-1,R*Step)],
    Labels=[time_label(maps:get(start,P),R,Context) || P <- Points],
    Total=#{label=>?__("Total",Context),color=><<"#00a6e8">>,width=>3,
        data=>series_points(Points,Labels,fun(P)->maps:get(total,P) end)},
    Keys=maps:from_keys([maps:get(key,G) || G <- Groups, maps:get(key,G) =/= <<"$other">>],true),
    Breakdown=case maps:get(dimension,Col,undefined) of
        undefined -> [];
        <<>> -> [];
        _ -> [#{label=>maps:get(label,G),width=>1,
            data=>series_points(Points,Labels,fun(P) ->
                Counts=maps:get(counts,P),
                case maps:get(key,G) of
                    <<"$other">> -> maps:fold(fun(K,V,A) ->
                        case maps:is_key(K,Keys) of true -> A; false -> A+V end
                    end,0,Counts);
                    Key -> maps:get(Key,Counts,0)
                end
            end)} || G <- Groups]
    end,
    [Total | Breakdown].

series_points(Points,Labels,Value) ->
    [begin
        V=Value(P),
        #{label=>L,value=>case maps:get(complete,P) orelse V>0 of
            true -> V; false -> null
        end}
    end || {P,L} <- lists:zip(Points,Labels)].


time_label(T,R,Context) ->
    Date=z_datetime:timestamp_to_datetime(T),
    Format=case R of 60 -> "H:i"; 900 -> "D H:i"; 3600 -> "M j H:i"; _ -> "Y-m-d" end,
    z_datetime:format(Date,Format,Context).
point(A,B,R,Index) ->
    Rows=[maps:get(T,Index,#{counts=>#{},complete=>false})||T<-lists:seq(A,B-1,R)],
    Counts=lists:foldl(fun(X,Acc)->z_analytics:merge(Acc,maps:get(counts,X)) end,#{},Rows),
    #{start=>A,counts=>Counts,total=>z_analytics_frequency:total(Counts),
      complete=>lists:all(fun(X)->maps:get(complete,X) end,Rows)}.
