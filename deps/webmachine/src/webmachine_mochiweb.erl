%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2008 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%% @doc Mochiweb interface for webmachine.
-module(webmachine_mochiweb).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-export([start/1, stop/0, loop/1]).
-export([t/0, t2/0, t3/0]).

-include("webmachine_logger.hrl").
-include_lib("wm_reqdata.hrl").

start(Options) ->
    {DispatchList, Options1} = get_option(dispatch, Options),
    {ErrorHandler0, Options2} = get_option(error_handler, Options1),
    {EnablePerfLog, Options3} = get_option(enable_perf_logger, Options2),
    ErrorHandler = 
        case ErrorHandler0 of 
            undefined ->
                webmachine_error_handler;
            EH -> EH
        end,
    {LogDir, Options4} = get_option(log_dir, Options3),
    webmachine_sup:start_logger(LogDir),
    case EnablePerfLog of
        true ->
            application:set_env(webmachine, enable_perf_logger, true),
            webmachine_sup:start_perf_logger(LogDir);
        _ ->
            ignore
    end,
    application:set_env(webmachine, dispatch_list, DispatchList),
    application:set_env(webmachine, error_handler, ErrorHandler),
    mochiweb_http:start([{name, ?MODULE}, {loop, fun loop/1} | Options4]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(MochiReq) ->
    %?DBG(MochiReq),
    ReqData = webmachine:init_reqdata(mochiweb, MochiReq),
    Host = case host_headers(ReqData) of
               [H|_] -> H;
               [] -> []
           end,
    Path = wrq:path(ReqData),
    {Dispatch, ReqDispatch} = case application:get_env(webmachine, dispatcher) of
        {ok, Dispatcher} ->
            Dispatcher:dispatch(Host, Path, ReqData);
        undefined ->
            {ok, DispatchList} = application:get_env(webmachine, dispatch_list),
            {webmachine_dispatcher:dispatch(Host, Path, DispatchList), ReqData}
    end,
    case Dispatch of
        {no_dispatch_match, _UnmatchedHost, _UnmatchedPathTokens} ->
            {ok, ErrorHandler} = application:get_env(webmachine, error_handler),
            {ErrorHTML,ReqState1} = ErrorHandler:render_error(404, ReqDispatch, {none, none, []}),
            {ok,ReqState2} = webmachine_request:append_to_response_body(ErrorHTML, ReqState1),
            {ok,ReqState3} = webmachine_request:send_response(404, ReqState2),
            LogData = webmachine_request:log_data(ReqState3),
            case application:get_env(webmachine,webmachine_logger_module) of
                {ok, LogModule} ->
                    spawn(LogModule, log_access, [LogData]);
                _ -> nop
            end;
        {Mod, ModOpts, HostTokens, Port, PathTokens, Bindings, AppRoot, StringPath} ->
            BootstrapResource = webmachine_resource:new(x,x,x,x),
            {ok, Resource} = BootstrapResource:wrap(Mod, ModOpts),
            {ok,RD1} = webmachine_request:load_dispatch_data(Bindings,HostTokens,Port,PathTokens,AppRoot,StringPath,ReqDispatch),
            {ok,RD2} = webmachine_request:set_metadata('resource_module', Mod, RD1),
            Result = try 
				case webmachine_decision_core:handle_request(Resource, RD2) of
	                {_, RsFin, RdFin} ->
		                EndTime = now(),
		                {_, RdResp} = webmachine_request:send_response(RdFin),
		                LogData0 = webmachine_request:log_data(RdResp),
		                spawn(fun() -> webmachine_decision_core:do_log(LogData0#wm_log_data{resource_module=Mod, end_time=EndTime}) end),
		                RsFin:stop(),
						ok;
					{upgrade, UpgradeFun, RsFin, RdFin} ->
		                RsFin:stop(),
						{upgrade, UpgradeFun, RdFin}
				end
            catch
                error:_ -> 
                    ?DBG({error, erlang:get_stacktrace()}),
                    {ok,RD3} = webmachine_request:send_response(500, RD2),
                    case application:get_env(webmachine, webmachine_logger_module) of
                        {ok, LogModule} ->
                            spawn(LogModule, log_access, [webmachine_request:log_data(RD3)]);
                        _ -> nop
                    end
            end,
			
			%% Optional tail continuation to a function that takes over the request.
			%% Used for protocol upgrades.
			case Result of
				ok -> ok;
				{upgrade, Fun, RdUpgrade} -> Mod:Fun(RdUpgrade)
			end;
        handled ->
            nop
    end.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

host_headers(ReqData) ->
    [ V || V <- [wrq:get_req_header_lc(H, ReqData)
                             || H <- ["x-forwarded-host",
                                      "x-forwarded-server",
                                      "host"]],
           V /= undefined].
           
           



t() ->
    MochiReq =  {mochiweb_request, undefined, 'GET',
                                    "/helloworld",
                                    {1,1},
                                    {7,
                                     {"host",
                                      {'Host',"127.0.0.1:8000"},
                                      {"accept",
                                       {'Accept',
                                        "application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"},
                                       nil,
                                       {"accept-language",
                                        {'Accept-Language',"en-us"},
                                        {"accept-encoding",
                                         {'Accept-Encoding',"gzip, deflate"},
                                         nil,nil},
                                        {"cookie",
                                         {'Cookie',"z_pid=JosyWAQNrnxcrom9NaGC"},
                                         {"connection",
                                          {'Connection',"keep-alive"},
                                          nil,nil},
                                         nil}}},
                                      {"user-agent",
                                       {'User-Agent',
                                        "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_2; en-us) AppleWebKit/531.21.8 (KHTML, like Gecko) Version/4.0.4 Safari/531.21.10"},
                                       nil,nil}}}},
    loop(MochiReq).


t2() ->
    MochiReq = {mochiweb_request,undefined,'GET',
									"/media/inline/koe.jpg",
                                    %"/lib/images/dropdown.jpg",
                                    %"/lib/js/apps/zotonic-1.0.js",
                                    {1,1},
                                    {8,
                                     {"host",
                                      {'Host',"127.0.0.1:8000"},
                                      {"cache-control",
                                       {'Cache-Control',"max-age=0"},
                                       {"accept",
                                        {'Accept',
                                         "application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"},
                                        nil,
                                        {"accept-language",
                                         {'Accept-Language',"en-us"},
                                         {"accept-encoding",
                                          {'Accept-Encoding',"gzip, deflate"},
                                          nil,nil},
                                         nil}},
                                       {"cookie",
                                        {'Cookie',"z_pid=JosyWAQNrnxcrom9NaGC"},
                                        {"connection",
                                         {'Connection',"keep-alive"},
                                         nil,nil},
                                        nil}},
                                      {"user-agent",
                                       {'User-Agent',
                                        "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_2; en-us) AppleWebKit/531.21.8 (KHTML, like Gecko) Version/4.0.4 Safari/531.21.10"},
                                       nil,nil}}}},
    loop(MochiReq).


t3() ->
	Headers = [
		{"Upgrade", "WebSocket"},
		{"Connection", "Upgrade"},
		{"Host", "example.com"},
		{"Origin", "http://example.com"},
		{"WebSocket-Protocol", "sample"}
	],
    MochiReq = {mochiweb_request,undefined,'GET',
									"/websocket",
                                    {1,1},
									mochiweb_headers:make(Headers)
				},
    loop(MochiReq).

