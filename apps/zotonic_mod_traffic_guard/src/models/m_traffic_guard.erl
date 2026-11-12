%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Administrator-managed IP exceptions for the traffic guard.
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

-module(m_traffic_guard).
-behaviour(zotonic_model).
-export([m_get/3,m_post/3,m_delete/3,lists/1,put/5,delete/2,throttled/1,block/3,
    stored/1,install/1,cleanup/1,parse/1,normalize/1,validity/2,global/0]).
-include_lib("zotonic_core/include/zotonic.hrl").
-moduledoc(#{zotonic_keywords => ["reference", "model", "security", "configuration"]}).
-moduledoc("
Administrator-only traffic guard rules and current rate-limited IPs.

GET `lists` returns database and global entries with IP, kind, note, expiry (Unix
seconds, zero means forever), and source. GET `throttled` returns node-local active
rate-limit decisions, including observe-mode decisions, without visited URLs.
POST `entry` accepts `kind` (allow/block), `ips` (whitespace/comma separated), `note`
and `validity` (hour/day/week/month/forever or YYYY-MM-DDTHH:MM in UTC). Re-adding
an IP of the same kind updates its note and expiry. POST `block` accepts `ip` and
`validity`, and generates a note from current throttling evidence. DELETE `entry/id`
removes a database entry. All mutations reject read-only credentials.

At most 256 database entries per kind are allowed. Notes are required, at most
1000 characters. Global `traffic_guard_allow` and `traffic_guard_block` lists in
zotonic.config are merged on every configuration load and cannot be edited here.
Allow takes precedence. Expired rules stop matching immediately and are deleted
by hourly maintenance. No migration from the former JSON setting is performed.
").

m_get([<<"lists">>|Rest],_,C) -> result(lists(C),Rest);
m_get([<<"throttled">>|Rest],_,C) -> result(throttled(C),Rest);
m_get(_,_,_) -> {error,unknown_path}.
result({ok,V},Rest) -> {ok,{V,Rest}};
result(Error,_) -> Error.
m_post([<<"entry">>],#{payload:=P},C) when is_map(P) ->
    put(maps:get(<<"kind">>,P,undefined),maps:get(<<"ips">>,P,undefined),
        maps:get(<<"note">>,P,undefined),maps:get(<<"validity">>,P,undefined),C);
m_post([<<"block">>],#{payload:=P},C) when is_map(P) ->
    block(maps:get(<<"ip">>,P,undefined),maps:get(<<"validity">>,P,undefined),C);
m_post(_,_,_) -> {error,unknown_path}.
m_delete([<<"entry">>,Id],_,C) -> delete(Id,C);
m_delete(_,_,_) -> {error,unknown_path}.

install(C) ->
    z_db:q("create table if not exists traffic_guard_ip (
        id serial primary key,
        kind varchar(5) not null check (kind in ('allow','block')),
        ip inet not null,
        note varchar(1000) not null,
        expires bigint not null default 0 check (expires >= 0),
        created timestamptz not null default now(),
        unique(kind,ip))",C),
    ok.
cleanup(C) ->
    z_db:q("delete from traffic_guard_ip where expires > 0 and expires <= $1",
        [erlang:system_time(second)],C),
    ok.

lists(C) ->
    case z_acl:is_admin(C) of
        true ->
            Rows=entries(C) ++ global(),
            {ok,lists:sort(fun(A,B) ->
                {maps:get(kind,A),maps:get(ip,A),maps:get(source,A)} <
                {maps:get(kind,B),maps:get(ip,B),maps:get(source,B)}
            end,Rows)};
        false -> {error,eacces}
    end.
entries(C) ->
    {ok,Rows}=z_db:qmap("select id,kind,host(ip) as ip,note,expires from traffic_guard_ip
        where expires=0 or expires>$1 order by kind,ip",[erlang:system_time(second)],[{keys,atom}],C),
    [R#{source=><<"site">>,until=>until_text(maps:get(expires,R))} || R<-Rows].

%% Trusted internal read. Expiry accompanies the cached rule, so no timer or
%% database roundtrip is needed to stop applying it at its deadline.
stored(C) ->
    Rows=entries(C) ++ global(),
    maps:from_list([{K,[{IP,maps:get(expires,R)} || R<-Rows,
        maps:get(kind,R)=:=atom_to_binary(K,utf8),
        {ok,[IP]}<-[parse(maps:get(ip,R))]]} || K<-[allow,block]]).

global() ->
    global_entries(<<"allow">>,z_config:get(traffic_guard_allow,[])) ++
    global_entries(<<"block">>,z_config:get(traffic_guard_block,[])).
global_entries(Kind,Values) when is_list(Values) ->
    lists:flatmap(fun(V) ->
        {Address,Note}=case V of
            #{ip:=Addr,note:=N} -> {Addr,N};
            _ -> {V,<<"Configured in zotonic.config">>}
        end,
        try {parse(z_convert:to_binary(Address)),valid_note(z_convert:to_binary(Note))} of
            {{ok,IPs},{ok,Text}} -> [#{kind=>Kind,ip=>ip_text(IP),note=>Text,
                expires=>0,until=><<>>,source=><<"zotonic.config">>} || IP<-IPs];
            _ -> []
        catch _:_ -> [] end
    end,Values);
global_entries(_,_) -> [].

put(Kind,IPs,Note,Validity,C) ->
    case writable(C) of
        false -> {error,eacces};
        true ->
            case {valid_kind(Kind),parse(IPs),valid_note(Note),validity(Validity,erlang:system_time(second))} of
                {true,{ok,[_|_]=Addresses},{ok,N},{ok,Until}} ->
                    changed(z_db:transaction(fun(Tx) ->
                        z_db:q("lock table traffic_guard_ip in share row exclusive mode",Tx),
                        cleanup(Tx),
                        lists:foreach(fun(IP) ->
                            z_db:q("insert into traffic_guard_ip (kind,ip,note,expires)
                                values ($1,$2::inet,$3,$4) on conflict (kind,ip)
                                do update set note=excluded.note,expires=excluded.expires",
                                [Kind,IP,N,Until],Tx)
                        end,Addresses),
                        case z_db:q1("select count(*) from traffic_guard_ip where kind=$1",[Kind],Tx) of
                            Count when Count =< 256 -> ok;
                            _ -> throw({rollback,too_many_entries})
                        end
                    end,C),C);
                _ -> {error,invalid_entry}
            end
    end.

valid_kind(<<"allow">>) -> true;
valid_kind(<<"block">>) -> true;
valid_kind(_) -> false.
valid_note(N) when is_binary(N), byte_size(N)=<4000 ->
    try string:trim(N) of
        <<>> -> error;
        T -> case string:length(T)=<1000 of true->{ok,T}; false->error end
    catch _:_ -> error end;
valid_note(_) -> error.
writable(C) -> z_acl:is_admin(C) andalso not z_acl:is_read_only(C).
changed(ok,C) -> z_traffic_guard:reload_rules(C), ok;
changed({rollback,Reason},_) -> {error,Reason};
changed(Error,_) -> Error.

delete(Id,C) ->
    case writable(C) of
        false -> {error,eacces};
        true ->
            case id(Id) of
                I when is_integer(I),I>0 ->
                    z_db:q("delete from traffic_guard_ip where id=$1",[I],C),
                    changed(ok,C);
                _ -> {error,badarg}
            end
    end.
id(I) when is_integer(I) -> I;
id(B) when is_binary(B),byte_size(B)<12 -> try binary_to_integer(B) catch _:_ -> undefined end;
id(_) -> undefined.

throttled(C) ->
    case z_acl:is_admin(C) of
        true -> z_traffic_guard:throttled(C);
        false -> {error,eacces}
    end.
block(IP,Validity,C) ->
    case writable(C) of
        false -> {error,eacces};
        true ->
            case {parse(IP),throttled(C)} of
                {{ok,[Address]},{ok,Rows}} ->
                    Text=ip_text(Address),
                    case [R || R<-Rows,maps:get(ip,R)=:=Text] of
                        [R|_] ->
                            Note=iolist_to_binary(io_lib:format("Traffic guard: ~s; ~B rate-limit decisions; added ~s UTC",
                                [maps:get(reason,R),maps:get(count,R),until_text(erlang:system_time(second))])),
                            put(<<"block">>,Text,Note,Validity,C);
                        [] -> {error,not_throttled}
                    end;
                _ -> {error,badarg}
            end
    end.

-spec validity(Value,Now) -> {ok,non_neg_integer()} | {error,invalid_expiry}
    when Value :: term(), Now :: integer().
validity(<<"forever">>,_) -> {ok,0};
validity(<<"hour">>,Now) -> {ok,Now+3600};
validity(<<"day">>,Now) -> {ok,Now+86400};
validity(<<"week">>,Now) -> {ok,Now+7*86400};
validity(<<"month">>,Now) -> {ok,Now+30*86400};
validity(<<Y:4/binary,"-",M:2/binary,"-",D:2/binary,"T",H:2/binary,":",Min:2/binary>>,Now) ->
    try
        Date={binary_to_integer(Y),binary_to_integer(M),binary_to_integer(D)},
        Hour=binary_to_integer(H), Minute=binary_to_integer(Min),
        true=calendar:valid_date(Date), true=Hour>=0 andalso Hour<24,
        true=Minute>=0 andalso Minute<60,
        Until=calendar:datetime_to_gregorian_seconds({Date,{Hour,Minute,0}})-62167219200,
        true=Until>Now,
        {ok,Until}
    catch _:_ -> {error,invalid_expiry} end;
validity(_,_) -> {error,invalid_expiry}.
until_text(0) -> <<>>;
until_text(N) ->
    {{Y,M,D},{H,I,_}}=calendar:system_time_to_universal_time(N,second),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B",[Y,M,D,H,I])).
ip_text(IP) -> list_to_binary(inet:ntoa(IP)).

-spec parse(Value) -> {ok,[inet:ip_address()]} | {error,invalid_ip_list} when Value :: term().
parse(Value) when is_binary(Value), byte_size(Value) =< 16384 ->
    case re:run(Value, <<"\\A[0-9a-fA-F:., \\t\\r\\n]*\\z">>, [{capture,none}]) of
        match -> parse_text(Value);
        nomatch -> {error,invalid_ip_list}
    end;
parse(_) -> {error,invalid_ip_list}.

parse_text(Value) ->
    Lines = [IP || IP <- re:split(Value, <<"[, \\t\\r\\n]+">>, [{return,binary}]), IP =/= <<>>],
    case length(Lines) =< 256 of
        true -> parse_lines(Lines,[]);
        false -> {error,invalid_ip_list}
    end.
parse_lines([],Acc) -> {ok,lists:usort(Acc)};
parse_lines([Line|Rest],Acc) ->
    case inet:parse_strict_address(binary_to_list(Line)) of
        {ok,IP} -> parse_lines(Rest,[normalize(IP)|Acc]);
        _ -> {error,invalid_ip_list}
    end.

%% IPv4-mapped IPv6 must not bypass an IPv4 entry.
normalize({0,0,0,0,0,16#ffff,A,B}) -> {A bsr 8,A band 255,B bsr 8,B band 255};
normalize(IP) -> IP.

