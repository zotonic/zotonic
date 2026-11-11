%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Request eligibility, dispatch classification and privacy-safe dimensions.
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

-module(z_analytics_request).
-moduledoc("Request policy and privacy boundary. Only coarse approved dimensions leave this module.").
-export([mark/1, allowed/1, dimensions/1, request_event/1, typed_event/1, dispatch_types/1, dispatch_type/2, traffic/2]).

-spec mark(Context) -> z:context() when Context :: z:context().
mark(Context) ->
    try
        case dispatch_type(z_context:get(analytics,Context), z_context:get_controller_module(Context)) of
            undefined -> ok;
            Type ->
                case interested(Type,Context) of
                    false -> ok;
                    true when Type =:= <<"page">> -> mark_page(Context);
                    true -> mark_request(Type,Context)
                end
        end
    catch _:_ -> ok end,
    Context.

%% Skip ACL, resource, user-agent and GeoIP work when no recording consumes
%% this request stream. Configuration reloads publish interest via ETS.
interested(Type,Context) ->
    z_analytics_worker:interested(<<"request">>,Context) orelse
        case Type of
            <<"page">> -> z_analytics_worker:interested(<<"page_request">>,Context);
            <<"api">> -> z_analytics_worker:interested(<<"api_request">>,Context);
            _ -> false
        end.

%% Standard controller defaults preserve existing sites. Explicit dispatch options
%% take precedence, including false to disable a route.
-spec dispatch_type(term(), atom()) -> binary() | undefined.
dispatch_type(false, _) -> undefined;
dispatch_type(undefined, controller_template) -> <<"page">>;
dispatch_type(undefined, controller_page) -> <<"page">>;
dispatch_type(undefined, controller_api) -> <<"api">>;
dispatch_type(undefined, _) -> undefined;
dispatch_type(Type, _) when Type =:= page; Type =:= api; Type =:= download; Type =:= asset ->
    atom_to_binary(Type,utf8);
dispatch_type(Type, _) when is_atom(Type) -> dispatch_type(atom_to_binary(Type,utf8), undefined);
dispatch_type(Type, _) when is_binary(Type), byte_size(Type) > 0, byte_size(Type) =< 64 ->
    case re:run(Type, <<"^[a-z][a-z0-9_]*$">>, [{capture,none}]) of
        match -> Type;
        nomatch -> undefined
    end;
dispatch_type(_, _) -> undefined.

-spec dispatch_types(z:context()) -> [binary()].
dispatch_types(Context) ->
    case z_dispatcher:dispatchinfo(Context) of
        {ok,#{dispatch_list:=Rules}} ->
            lists:usort([T || {_,_,Controller,Options} <- Rules,
                T <- [dispatch_type(proplists:get_value(analytics,Options),Controller)],
                T =/= undefined]);
        _ -> []
    end.

mark_request(Type,Context) ->
    case allowed(Context) of
        true ->
            D = (maps:with([<<"language">>, <<"country">>, <<"traffic">>, <<"crawl_behavior">>, <<"guard_action">>, <<"guard_reason">>], dimensions(Context)))#{
                <<"has_page_id">>=>has_page_id(page_id(Context))},
            Metrics = #{analytics_request=>D#{<<"type">>=>Type}},
            case Type of
                <<"api">> -> z_context:set_req_metrics(Metrics#{analytics_api=>D},Context);
                _ -> z_context:set_req_metrics(Metrics,Context)
            end;
        false -> ok
    end.

mark_page(Context) ->
        Key = page_id(Context),
        CT = z_context:get_resp_header(<<"content-type">>,Context),
        IsHtml = is_binary(CT) andalso binary:match(CT,<<"text/html">>) =:= {0,9},
        case allowed(Context) andalso IsHtml andalso public(Key,Context) of
            true ->
                D0 = (dimensions(Context))#{<<"has_page_id">>=>has_page_id(Key)},
                D = case Key of
                    undefined -> D0;
                    _ -> D0#{<<"page">>=>z_convert:to_binary(Key)}
                end,
                z_context:set_req_metrics(#{analytics=>D,
                    analytics_request=>(maps:with([<<"language">>,<<"country">>,<<"traffic">>,<<"has_page_id">>,<<"crawl_behavior">>,<<"guard_action">>,<<"guard_reason">>],D))#{<<"type">>=><<"page">>}},Context);
            false -> ok
        end.

%% Use the completed response status, not a provisional controller response.
%% No API topic/path or request data crosses this boundary.
-spec request_event(map()) -> {binary(), map()} | undefined.
request_event(#{method:=Method,resp_status:=Status,metrics:=#{analytics_api:=D}})
        when is_integer(Status), Status >= 100, Status < 600 ->
    M = case lists:member(Method,[<<"GET">>,<<"HEAD">>,<<"POST">>,<<"PUT">>,
            <<"PATCH">>,<<"DELETE">>,<<"OPTIONS">>]) of
        true -> Method;
        false -> <<"OTHER">>
    end,
    {
        <<"api_request">>,
        D#{
            <<"method">>=>M,
            <<"http_status">>=>integer_to_binary(Status),
            <<"status">> => <<(integer_to_binary(Status div 100))/binary,"xx">>
        }
    };
request_event(#{method:= <<"GET">>,resp_status:=200,metrics:=#{analytics:=D}}) ->
    {<<"page_request">>,D};
request_event(_) ->
    undefined.

%% Page requests retain their successful public HTML GET definition. Other
%% explicitly classified routes count completed responses, including errors.
-spec typed_event(map()) -> {binary(), map()} | undefined.
typed_event(#{method:=Method,resp_status:=Status,metrics:=#{analytics_request:=D}}=P) ->
    case maps:get(<<"type">>,D) =:= <<"page">> andalso
            not (Method =:= <<"GET">> andalso Status =:= 200) of
        true -> undefined;
        false ->
            case request_event(P#{metrics=>#{analytics_api=>D}}) of
                {_,Props} -> {<<"request">>,Props};
                undefined -> undefined
            end
    end;
typed_event(_) ->
    undefined.

%% controller_template renders only the configured resource; a query-string id
%% must not change whether an unrelated template is considered a resource page.
page_id(Context) ->
    case z_context:get(analytics_page,Context) of
        undefined ->
            case z_context:get_controller_module(Context) of
                controller_template -> z_controller_helper:get_configured_id(Context);
                _ -> z_controller_helper:get_id(Context)
            end;
        Id -> Id
    end.

has_page_id(Id) when is_integer(Id) -> <<"true">>;
has_page_id(_) -> <<"false">>.

public(undefined,Context) ->
    %% Do not turn an invalid configured resource into a resource-less page.
    case {z_context:get(id,Context),z_context:get(analytics_page,Context)} of
        {undefined,undefined} ->
            {Allowed,_} = z_controller_helper:is_authorized(undefined,z_acl:anondo(Context)),
            Allowed;
        _ -> false
    end;
public(Id,Context) when is_integer(Id) ->
    m_rsc:is_visible(Id,z_acl:anondo(Context));
public(_,_) ->
    false.

-spec allowed(Context) -> boolean() when Context :: z:context().
allowed(Context) ->
    not z_convert:to_bool(z_context:get(analytics_exclude,Context))
        andalso not z_acl:is_allowed(use,mod_admin,Context)
        andalso (not m_config:get_boolean(mod_analytics,require_consent,Context)
            orelse z_context:get(analytics_consent,Context) =:= true).

-spec dimensions(Context) -> map() when Context :: z:context().
dimensions(Context) ->
    UA = case m_req:get(user_agent,Context) of B when is_binary(B), byte_size(B)=<2048 -> string:lowercase(B); _ -> <<>> end,
    Traffic = traffic(UA, z_user_agent:is_crawler(Context)),
    (guard_signals(Context))#{
        <<"language">>=>z_convert:to_binary(z_context:language(Context)),
        <<"device">>=>device(UA),
        <<"traffic">>=>Traffic,
        <<"country">>=>country(Context),
        <<"referrer">>=>referrer(cowmachine_req:get_req_header(<<"referer">>,Context),m_req:get(host,Context))
    }.

%% Guard context is server-owned; never copy client/session identifiers.
guard_signals(Context) ->
    case z_context:get(crawl_guard_result, Context) of
        #{crawl_behavior := B, guard_action := A, guard_reason := R} ->
            #{<<"crawl_behavior">> => B, <<"guard_action">> => A, <<"guard_reason">> => R};
        _ -> #{}
    end.

%% Declared product tokens are not verified identities. Match token boundaries,
%% not arbitrary substrings; normal browser automation remains unrecognized.
-spec traffic(UserAgent, IsCrawler) -> binary() when
    UserAgent :: term(),
    IsCrawler :: boolean().
traffic(UA, IsCrawler) when is_binary(UA), byte_size(UA) > 0, byte_size(UA) =< 2048 ->
    Tokens = string:lowercase(UA),
    case product_token(Tokens, <<"chatgpt-user|claude-user">>) of
        true -> <<"agent">>;
        false ->
            case IsCrawler orelse product_token(Tokens,
                    <<"gptbot|oai-searchbot|claudebot|claude-searchbot">>) of
                true -> <<"crawler">>;
                false -> <<"other">>
            end
    end;
traffic(_, true) -> <<"crawler">>;
traffic(_, false) -> <<"unknown">>.

product_token(UA, Products) ->
    re:run(UA, <<"(?:^|[\\s;(])(?:", Products/binary, ")(?:/|[\\s;)]|$)">>,
        [{capture, none}]) =:= match.

device(<<>>) ->
    <<"unknown">>;
device(UA) ->
    case {contains(UA,[<<"ipad">>,<<"tablet">>]),contains(UA,[<<"mobile">>,<<"iphone">>]),contains(UA,[<<"mozilla/">>])} of
        {true,_,_}-> <<"tablet">>;
        {_,true,_}-> <<"phone">>;
        {_,_,true}-> <<"desktop">>;
        _-> <<"unknown">>
    end.

contains(B,Needles) -> lists:any(fun(N)->binary:match(B,N)=/=nomatch end,Needles).

referrer(B,Host) when is_binary(B), byte_size(B)=<4096 ->
    try
        #{host:=H0,scheme:=Scheme} = uri_string:parse(B),
        true = Scheme =:= <<"http">> orelse Scheme =:= <<"https">>,
        H = string:lowercase(H0), true = byte_size(H)=<253,
        case H =:= Host of true -> <<"internal">>; false -> H end
    catch
        _:_ -> <<"direct/unknown">>
    end;
referrer(_,_) ->
    <<"direct/unknown">>.

%% Optional local GeoIP database; no network request is performed by this lookup.
country(Context) ->
    try
        true=z_module_manager:active(mod_geoip,Context),
        case filter_ip2country:ip2country(m_req:get(peer_ip,Context),Context) of
            B when is_binary(B),byte_size(B)=:=2 -> B;
            _ -> <<"unknown">>
        end
    catch
        _:_ -> <<"unknown">>
    end.
