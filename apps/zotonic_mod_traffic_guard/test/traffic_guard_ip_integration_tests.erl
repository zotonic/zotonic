%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Traffic guard admission, privacy and recovery regression tests.
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

-module(traffic_guard_ip_integration_tests).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

%% Explicit development-site test. Reserved test IPs must not already have rules.
run(C) ->
    development=m_site:environment(C),
    true=z_module_manager:active(mod_traffic_guard,C),
    IPs=[<<"127.0.0.1">>,<<"::1">>,<<"198.51.100.98">>],
    ?assertEqual(0,z_db:q1("select count(*) from traffic_guard_ip where host(ip)=any($1::text[])",[IPs],C)),
    Original=[{K,m_config:get_value(mod_traffic_guard,K,C)} || K<-[mode,period,request_burst,request_limit]],
    Global=[{K,application:get_env(zotonic,K)} || K<-[traffic_guard_allow,traffic_guard_block]],
    A=z_acl:sudo(C), Loopback = <<"127.0.0.1, ::1">>,
    URL=z_convert:to_list(z_context:abs_url(z_dispatcher:url_for(home,C),C)),
    try
        configure([{mode,<<"enforce">>},{period,<<"60">>},{request_limit,<<"120">>},{request_burst,<<"30">>}],C),
        ok=m_traffic_guard:put(<<"block">>,Loopback,<<"Integration block">>,<<"day">>,A),
        {429,H,_}=request(URL),
        ?assertEqual("no-store",proplists:get_value("cache-control",H)),
        configure([{mode,<<"observe">>}],C), ?assertMatch({200,_,_},request(URL)),
        configure([{mode,<<"off">>}],C), ?assertMatch({200,_,_},request(URL)),
        configure([{mode,<<"enforce">>}],C),
        ok=m_traffic_guard:put(<<"allow">>,Loopback,<<"Integration allow">>,<<"hour">>,A),
        ?assertMatch({200,_,_},request(URL)),
        {ok,Rows}=m_traffic_guard:lists(A),
        [Rule]=[R || R<-Rows,maps:get(kind,R)=:=<<"allow">>,maps:get(ip,R)=:=<<"127.0.0.1">>,maps:get(source,R)=:=<<"site">>],
        Id=maps:get(id,Rule),
        ok=m_traffic_guard:put(<<"allow">>,<<"127.0.0.1">>,<<"Updated <script> note">>,<<"forever">>,A),
        ?assertEqual(0,z_db:q1("select expires from traffic_guard_ip where id=$1",[Id],C)),
        ?assertEqual({error,invalid_entry},m_traffic_guard:put(<<"allow">>,<<"127.0.0.1">>,<<>>,<<"forever">>,A)),
        ?assertEqual(<<"Updated <script> note">>,z_db:q1("select note from traffic_guard_ip where id=$1",[Id],C)),
        ReadOnly=C#context{user_id=1,acl=undefined,acl_is_read_only=true},
        ?assertMatch({ok,_},m_traffic_guard:lists(ReadOnly)),
        ?assertEqual({error,eacces},m_traffic_guard:delete(Id,ReadOnly)),
        ?assertEqual({error,eacces},m_traffic_guard:put(<<"block">>,Loopback,<<"Denied">>,<<"day">>,ReadOnly)),
        ?assertEqual({error,eacces},m_traffic_guard:throttled(z_acl:anondo(C))),
        {ok,Edited}=m_traffic_guard:lists(A),
        Tree=z_template:render_block(content,"admin_traffic_guard.tpl",[{entries,Edited},{read_only,true}],ReadOnly),
        {Io,_}=z_render:render_to_iolist(Tree,ReadOnly), Html=iolist_to_binary(Io),
        ?assertEqual(nomatch,binary:match(Html,<<"<script> note">>)),
        ?assertEqual(nomatch,binary:match(Html,<<"<form">>)),
        %% Global lists are merged on every reload and cannot be deleted via row IDs.
        delete_kind(<<"allow">>,IPs,C),
        application:set_env(zotonic,traffic_guard_allow,["127.0.0.1","::1"]),
        z_traffic_guard:reset(C), ?assertMatch({200,_,_},request(URL)),
        {ok,G}=m_traffic_guard:lists(A), ?assert(lists:any(fun(R)->maps:get(source,R)=:=<<"zotonic.config">> end,G)),
        application:set_env(zotonic,traffic_guard_allow,[]),
        delete_kind(<<"block">>,IPs,C),
        application:set_env(zotonic,traffic_guard_block,["127.0.0.1","::1"]),
        z_traffic_guard:reset(C), ?assertMatch({429,_,_},request(URL)),
        application:set_env(zotonic,traffic_guard_block,[]),
        %% Expiry is honored from the cached copy without another reset.
        ok=m_traffic_guard:put(<<"block">>,Loopback,<<"Expires shortly">>,<<"hour">>,A),
        z_db:q("update traffic_guard_ip set expires=$1 where kind='block' and host(ip)=any($2::text[])",
            [erlang:system_time(second)+2,IPs],C),
        z_traffic_guard:reset(C), ?assertMatch({429,_,_},request(URL)),
        timer:sleep(2100), ?assertMatch({200,_,_},request(URL)),
        ok=m_traffic_guard:cleanup(C),
        ?assertEqual(0,z_db:q1("select count(*) from traffic_guard_ip where host(ip)=any($1::text[])",[IPs],C)),
        %% Promote current rate-limit evidence to a documented, timed block.
        configure([{request_burst,<<"1">>},{request_limit,<<"1">>}],C),
        [z_traffic_guard:check({198,51,100,98},z_traffic_guard:settings(C),C) || _<-lists:seq(1,3)],
        [z_traffic_guard:check({198,51,100,97},z_traffic_guard:settings(C),C) || _<-lists:seq(1,2)],
        {ok,T}=m_traffic_guard:throttled(A), ?assert(lists:any(fun(R)->maps:get(ip,R)=:=<<"198.51.100.98">> end,T)),
        ok=m_traffic_guard:block(<<"198.51.100.98">>,<<"week">>,A),
        {ok,Remaining}=m_traffic_guard:throttled(A),
        ?assert(lists:any(fun(R)->maps:get(ip,R)=:=<<"198.51.100.97">> end,Remaining)),
        ?assertNot(lists:any(fun(R)->maps:get(ip,R)=:=<<"198.51.100.98">> end,Remaining)),
        {ok,B}=m_traffic_guard:lists(A), [BR]=[R || R<-B,maps:get(ip,R)=:=<<"198.51.100.98">>],
        ?assertNotEqual(nomatch,binary:match(maps:get(note,BR),<<"request_rate">>)),
        ?assert(maps:get(expires,BR)>erlang:system_time(second)+6*86400),
        ok=m_traffic_guard:delete(maps:get(id,BR),A),
        #{database=>ok,expiry=>ok,global_merge=>ok,permissions=>ok,timed_block=>ok,http_modes=>ok}
    after
        z_db:q("delete from traffic_guard_ip where host(ip)=any($1::text[])",[IPs],C),
        [case V of undefined->m_config:delete(mod_traffic_guard,K,C); _->m_config:set_value(mod_traffic_guard,K,V,C) end || {K,V}<-Original],
        [case V of undefined->application:unset_env(zotonic,K); {ok,Val}->application:set_env(zotonic,K,Val) end || {K,V}<-Global],
        z_traffic_guard:reset(C)
    end.
delete_kind(Kind,IPs,C) ->
    z_db:q("delete from traffic_guard_ip where kind=$1 and host(ip)=any($2::text[])",[Kind,IPs],C),z_traffic_guard:reset(C).
configure(Pairs,C) ->
    [m_config:set_value(mod_traffic_guard,K,V,C) || {K,V}<-Pairs], z_traffic_guard:reset(C).
request(URL) ->
    {ok,{{_,Status,_},Headers,Body}}=httpc:request(get,{URL,[{"user-agent","Mozilla/5.0"}]},
        [{ssl,[{verify,verify_none}]},{timeout,10000},{autoredirect,false}],[]),
    {Status,Headers,Body}.
