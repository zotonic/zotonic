%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-03-19
%% @doc Resource to display traces of webmachine requests.

%% Copyright 2009 Marc Worrell
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
%%
%% Resource
%%

-module(resource_wmtrace).

-export([
    is_authorized/2,
    resource_exists/2
        ]).


-include_lib("resource_html.hrl").
-include_lib("webmachine_logger.hrl").

is_authorized(ReqData, Context) ->
    z_acl:wm_is_authorized(use, mod_development, ReqData, Context).

resource_exists(RD, Ctx) ->
    case wrq:disp_path(RD) of
        [] ->
            case lists:reverse(wrq:raw_path(RD)) of
                [$/|_] ->
                    {true, RD, Ctx};
                _ ->
                    {{halt, 303},
                     wrq:set_resp_header("Location",
                                         wrq:raw_path(RD)++"/",
                                         RD),
                     Ctx}
            end;
        TraceName ->
            Rootname = filename:rootname(filename:basename(TraceName)), 
            [{trace_dir, TraceDir}] = ets:lookup(?WMTRACE_CONF_TBL, trace_dir),
            TracePath = filename:join([TraceDir, Rootname ++ ".wmtrace"]),
            {filelib:is_file(TracePath), RD, z_context:set(trace, TracePath, Ctx)}
    end.
	
html(Context) ->
    case z_context:get(trace, Context) of
        undefined ->
            [{trace_dir, TraceDir}] = ets:lookup(?WMTRACE_CONF_TBL, trace_dir),
            Files = lists:reverse(
                lists:sort(
                filelib:fold_files(TraceDir,
                                   ".*\.wmtrace",
                                   false,
                                   fun(F, Acc) ->
                                           [filename:basename(F)|Acc]
                                   end,
                                   []))),
            Vars = [{files, Files}, {page_admin_wmtrace, true}],
            Html = z_template:render("wmtrace_list.tpl", Vars, Context),
            z_context:output(Html, Context);
        TraceFile ->
            Filename = filename:absname(TraceFile),
            {ok, Data} = file:consult(Filename),
            {Request, Response, {TrReqId, Trace}} = encode_trace(Data),
            Vars = [
                {filename, filename:basename(TraceFile)},
                {request, Request},
                {response, Response},
                {trace_req_id, TrReqId},
                {trace, Trace}
                   ],
            Html = z_template:render("wmtrace_diagram.tpl", Vars, Context),
            z_context:output(Html, Context)
    end.



%%
%% Trace Encoding
%%

encode_trace(Data) ->
    {Request, Response, {TrReqId, Trace}} = aggregate_trace(Data),
    {json_encode(encode_request(Request)),
     json_encode(encode_response(Response)),
     {TrReqId, json_encode({array, [ encode_trace_part(P) || P <- Trace ]})}}.

aggregate_trace(RawTrace) ->
    {TrReqId, RestRawTrace} = case RawTrace of        
                                  [{req_id, ReqId} | RestRawTrace_] -> 
                                      {ReqId, RestRawTrace_};
                                  RawTrace_ -> {undefined, RawTrace_}
                              end,
    {Request, Response, Trace} = lists:foldl(fun aggregate_trace_part/2,
                                             {undefined, 500, []},
                                             RestRawTrace),
    {Request, Response, {TrReqId, lists:reverse(Trace)}}.

aggregate_trace_part({decision, Decision}, {Q, R, Acc}) ->
    BDN = base_decision_name(Decision),
    case Acc of
        [{BDN,_}|_] -> {Q, R, Acc}; %% subdecision (ex. v3b13b)
        _ ->
            {Q, R, [{base_decision_name(Decision), []}|Acc]}
    end;
aggregate_trace_part({attempt, Module, Function, Args},
                     {Q, R, [{Decision,Calls}|Acc]}) ->
    {maybe_extract_request(Function, Args, Q),
     R, [{Decision,[{Module, Function, Args, wmtrace_null}|Calls]}|Acc]};
aggregate_trace_part({result, Module, Function, Result},
                     {Q, R, [{Decision,[{Module,Function,Args,_}|Calls]}|Acc]}) ->
    {Q, maybe_extract_response(Function, Result, R),
     [{Decision,[{Module, Function, Args, Result}|Calls]}|Acc]};
aggregate_trace_part({not_exported, Module, Function, Args},
                     {Q, R, [{Decision,Calls}|Acc]}) ->
    {Q, maybe_extract_response(Function, Args, R),
     [{Decision,[{Module, Function, Args, wmtrace_not_exported}|Calls]}
      |Acc]}.

maybe_extract_request(ping, [ReqData,_], _) ->
    ReqData;
maybe_extract_request(_, _, R) ->
    R.

maybe_extract_response(finish_request, [ReqData,_], _) ->
    ReqData;
maybe_extract_response(finish_request, {_, ReqData, _}, _) ->
    ReqData;
maybe_extract_response(_, _, R) ->
    R.

base_decision_name(v3b6_upgrade) ->
    "b6";
base_decision_name(Decision) ->
    [$v,$3|D] = atom_to_list(Decision), %% strip 'v3'
    case lists:reverse(D) of
        [A|RD] when A >= $a, A =< $z ->
            lists:reverse(RD); %% strip 'b' off end of some
        _ ->
            D
    end.

encode_request(ReqData) when is_record(ReqData, wm_reqdata) ->
    {struct, [{"method", atom_to_list(
        wrq:method(ReqData))},
              {"path", wrq:raw_path(ReqData)},
              {"headers", encode_headers(wrq:req_headers(ReqData))},
              {"body", case ReqData#wm_reqdata.req_body of
                           undefined -> [];
                           Body when is_atom(Body) ->
                               atom_to_list(Body);
                           Body -> lists:flatten(io_lib:format("~s", [Body]))
                       end}]}.
    
encode_response(ReqData) ->
    {struct, [{"code", integer_to_list(
        wrq:response_code(ReqData))},
              {"headers", encode_headers(wrq:resp_headers(ReqData))},
              {"body", lists:flatten(io_lib:format("~s", [wrq:resp_body(ReqData)]))}]}.

encode_headers(Headers) when is_list(Headers) ->
    {struct, [ {N, V} || {N, V} <- Headers ]};
encode_headers(Headers) ->
    encode_headers(mochiweb_headers:to_list(Headers)).

encode_trace_part({Decision, Calls}) ->
    {struct, [{"d", Decision},
              {"calls",
               {array, [ {struct,
                          [{"module", Module},
                           {"function", Function},
                           {"input", encode_trace_io(Input)},
                           {"output", encode_trace_io(Output)}]}
                        || {Module, Function, Input, Output}
                        <- lists:reverse(Calls) ]}}]}.

encode_trace_io(wmtrace_null) -> null;
encode_trace_io(wmtrace_not_exported) -> "wmtrace_not_exported";
encode_trace_io(Data) ->
    lists:flatten(io_lib:format("~p", [Data])).
    
    
%% Adapted mochijson:encode, also escapes $<

json_encode(true) ->
    "true";
json_encode(false) ->
    "false";
json_encode(null) ->
    "null";
json_encode(I) when is_integer(I) ->
    integer_to_list(I);
json_encode(F) when is_float(F) ->
    mochinum:digits(F);
json_encode(L) when is_list(L); is_binary(L); is_atom(L) ->
    json_encode_string(L);
json_encode({array, Props}) when is_list(Props) ->
    json_encode_array(Props);
json_encode({struct, Props}) when is_list(Props) ->
    json_encode_proplist(Props).

json_encode_array([]) ->
    "[]";
json_encode_array(L) ->
    F = fun (O, Acc) ->
                [$,, json_encode(O) | Acc]
        end,
    [$, | Acc1] = lists:foldl(F, "[", L),
    lists:reverse([$\] | Acc1]).

json_encode_proplist([]) ->
    "{}";
json_encode_proplist(Props) ->
    F = fun ({K, V}, Acc) ->
                KS = case K of 
                         K when is_atom(K) ->
                             json_encode_string(atom_to_list(K));
                         K when is_integer(K) ->
                             json_encode_string(integer_to_list(K));
                         K when is_list(K); is_binary(K) ->
                             json_encode_string(K)
                     end,
                VS = json_encode(V),
                [$,, VS, $:, KS | Acc]
        end,
    [$, | Acc1] = lists:foldl(F, "{", Props),
    lists:reverse([$\} | Acc1]).

json_encode_string(A) when is_atom(A) ->
    [$", z_utils:js_escape(atom_to_list(A)), $"];
json_encode_string(S) ->
    [$", z_utils:js_escape(S), $"].
