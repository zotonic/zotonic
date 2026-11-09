%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Bounded mergeable frequency summaries for popular content.
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

-module(z_analytics_frequency).
-moduledoc(<<"Bounded, mergeable weighted Misra-Gries summaries for popular content. Counters are lower bounds; error is a
deterministic per-key upper-bound offset. Total counts accepted events, including those no longer represented by a candidate.
Bucket expiry supplies the moving window; summaries are never subtracted.
"/utf8>>).
-export([enabled/1, new/0, add/4, merge/3, normalize/1, total/1, size/1, ranked/1, error/1]).

-spec enabled(map()) -> boolean().
enabled(#{dimension := <<"page">>}) -> true;
enabled(_) -> false.

-spec new() -> map().
new() -> #{format => mg1, counters => #{}, total => 0, error => 0}.

%% Older buckets stored exact retained counts plus anonymous overflow. Preserve
%% those lower bounds and account for all unidentified mass in the error bound.
-spec normalize(map()) -> map().
normalize(#{format := mg1}=S) -> S;
normalize(Counts) ->
    #{format => mg1, counters => maps:remove(<<"$other">>,Counts),
      total => total(Counts), error => maps:get(<<"$other">>,Counts,0)}.

-spec total(map()) -> non_neg_integer().
total(#{format := mg1, total := N}) -> N;
total(Counts) -> lists:sum(maps:values(Counts)).

-spec size(map()) -> non_neg_integer().
size(#{format := mg1, counters := C}) -> map_size(C);
size(C) -> map_size(C).

-spec error(map()) -> non_neg_integer().
error(S) -> maps:get(error,normalize(S)).

-spec add(binary(), pos_integer(), map(), pos_integer()) -> map().
add(Key,N,#{counters := C,total := Total,error := Error}=S,Limit) ->
    case maps:find(Key,C) of
        {ok,V} -> S#{counters=>C#{Key=>V+N},total=>Total+N};
        error when map_size(C) < Limit -> S#{counters=>C#{Key=>N},total=>Total+N};
        error ->
            %% Cancel equal weight from Limit+1 distinct keys. For unit events,
            %% this scans only on overflow, not on every event or known key.
            D=min(N,lists:min(maps:values(C))),
            C1=subtract(C,D),
            C2=case N>D of true -> C1#{Key=>N-D}; false -> C1 end,
            S#{counters=>C2,total=>Total+N,error=>Error+D}
    end.

%% Merge then subtract the (Limit+1)th largest weight. At most 2*Limit
%% candidates are materialized; reduction preserves every per-key bound.
-spec merge(map(), map(), pos_integer()) -> map().
merge(A0,B0,Limit) ->
    A=normalize(A0), B=normalize(B0),
    C=z_analytics:merge(maps:get(counters,A),maps:get(counters,B)),
    D=case map_size(C)>Limit of
        true -> lists:nth(Limit+1,lists:reverse(lists:sort(maps:values(C))));
        false -> 0
    end,
    #{format=>mg1,counters=>subtract(C,D),total=>total(A)+total(B),
      error=>maps:get(error,A)+maps:get(error,B)+D}.

subtract(C,0) -> C;
subtract(C,D) -> maps:filtermap(fun(_,V) ->
    case V>D of true -> {true,V-D}; false -> false end
end,C).

%% Deterministic ordering by lower bound, then key. Overlapping intervals do
%% not certify relative rank. Absent keys have a frequency at most Error.
-spec ranked(map()) -> [map()].
ranked(S0) ->
    #{counters:=C,total:=Total,error:=Error}=normalize(S0),
    Sorted=lists:sort(fun({K,A},{L,B})-> A>B orelse (A=:=B andalso K<L) end,maps:to_list(C)),
    [#{key=>K,count=>N,upper=>min(Total,N+Error)} || {K,N} <- Sorted].
