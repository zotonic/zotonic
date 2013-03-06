%% @doc Simple tool for ad-hoc tracing of Zotonic. Since the module 
%%      z_notifier, serves as an internal message router, tracing on 
%%      specific calls of the module is enough to monitor the internals
%%      of zotonic. Trace messages are logged to stdout by default, but
%%      a custom callback function can be specified.
%% @author Atilla Erdodi <atilla@maximonster.com>
%% @copyright 2010 Maximonster Interactive Things

-module(z_tracer).
-author('Atilla Erdodi <atilla@maximonster.com>').
-export([start/0, start/1, stop/0, get_tracer/0]). % API
-export([do_log/1]). % logger callback function 
-export([tracefun/2]). % dbg callback function

-include_lib("zotonic.hrl").
-include_lib("webzmachine/include/webmachine_logger.hrl").

-define(TRACED_MOD, z_notifier).
-define(TRACE_OPTS, []).

%% TODO: give information about receiving processes
%% TODO: do_log callback module for mod_development
%% TODO: use ttbe insead of dbg
%% TODO: multi node tracing


start() ->
    start(fun do_log/1).

start(DoLogCallback) when is_function(DoLogCallback) ->
    dbg:start(),
    dbg:tracer(process, {fun tracefun/2, {DoLogCallback, 0}}),
    lists:foreach(fun({F, A}) -> 
                          dbg:tp(?TRACED_MOD, F, A, ?TRACE_OPTS) 
                  end, traced_funs()),
    dbg:p(all, c).

stop() ->
    dbg:stop_clear().

get_tracer() ->
    dbg:get_tracer().
   
traced_funs() ->
    [{notify, 2}, {notify1, 2}, 
     {first, 2}, {map, 2},
     {foldl, 3}, {foldr, 3}].
   
ignored_events() ->
    [debug, tick_1s].

%% @doc Default callback function for tracefun/2
do_log({I, Pid, Fun, SessionPid, PagePid, ReqId, Msg}) ->
    io:format("(~p): ~p ~p :: ~p ~p ~p - ~p\n",
              [I, Pid, Fun, SessionPid, PagePid, ReqId, Msg]).

%% %doc Callback function for dbg
tracefun({trace, Pid, call, {?TRACED_MOD, Fun, Args}}, {DoLog, I}) ->
    {Msg, Context} = 
        case Args of 
            [Msg_, Context_] -> {Msg_, Context_};
            [Msg_, _Acc_, Context_] -> {Msg_, Context_}
        end,
    ReqData = z_context:get_reqdata(Context),
    ReqId = case ReqData of
                undefined -> undefined;
                        _ -> (ReqData#wm_reqdata.log_data)#wm_log_data.req_id
            end,
    EventType = if  is_tuple(Msg) -> element(1, Msg);
                    is_atom(Msg) -> Msg
                end,
    case lists:member(EventType, ignored_events()) of
        true ->
            {DoLog, I};
        false ->
            {SessionPid, PagePid, ReqId} = 
                {Context#context.session_pid,
                 Context#context.page_pid,
                 ReqId}, 
            DoLog({I, Pid, Fun, SessionPid, PagePid, ReqId, Msg}),
            {DoLog, I + 1}
    end;    
tracefun(_, {DoLog, I}) ->
    io:format("Ups!\n"),
    {DoLog, I}.
