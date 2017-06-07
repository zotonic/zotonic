%% @doc Cowboy stream handler for access and error logging.
-module(z_cowboy_log_stream_handler).
-author("David de Boer <david@ddeboer.nl>").

-behavior(cowboy_stream).

-export([
    init/3,
    data/4,
    info/3,
    terminate/3,
    early_error/5
]).

-record(state, {
    next :: any(),
    t_init :: any(),
    request :: map()
}).

-type state() :: #state{}.

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
	-> {cowboy_stream:commands(), {module(), state()} | undefined}.
init(StreamID, Req, Opts) ->
    %% TODO: can we pass a Zotonic logging callback fun in opts?
    {Commands, Next} = cowboy_stream:init(StreamID, Req, Opts),
    {Commands, #state{next = Next, t_init = os:timestamp(), request = Req}}.

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), binary(), {Handler, State} | undefined)
	-> {cowboy_stream:commands(), {Handler, State} | undefined}
	when Handler::module(), State::state().
data(StreamID, IsFin, Data, #state{next = Next} = State) ->
%%    z:debug_msg(?MODULE, ?LINE, Data),  %% Response body
    {Commands, Next1} = cowboy_stream:data(StreamID, IsFin, Data, Next),
    {Commands, State#state{next = Next1}}.

-spec info(cowboy_stream:streamid(), any(), {Handler, State} | undefined)
	-> {cowboy_stream:commands(), {Handler, State} | undefined}
	when Handler::module(), State::state().
info(StreamID, Response, #state{request = Request, next = Next} = State) ->
    case Response of
        {response, _Status, _Headers, _Body} -> 
            z_access_syslog:log_access(Request, Response);
        {headers, _Status, _Headers} -> 
            z_access_syslog:log_access(Request, Response);
        _ -> ignore
    end,

    {Commands, Next1} = cowboy_stream:info(StreamID, Response, Next),
    {Commands, State#state{next = Next1}}.


-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), {module(), state()} | undefined) -> ok.
terminate(StreamID, Reason, #state{next = Next}) ->
    cowboy_stream:terminate(StreamID, Reason, Next).

-spec early_error(cowboy_stream:streamid(), cowboy_stream:reason(), cowboy_stream:partial_req(), Resp, cowboy:opts())
	-> Resp when Resp::cowboy_stream:resp_command().
early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
    z:debug_msg(?MODULE, ?LINE, Reason),
    z:debug_msg(?MODULE, ?LINE, Resp),
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).


