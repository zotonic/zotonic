%% @doc Cowboy stream handler for access logging.
-module(z_cowboy_log_stream_handler).
-author("David de Boer <david@ddeboer.nl>").

-behavior(cowboy_stream).

-export([
    init/3,
    data/4,
    info/3,
    terminate/3
]).

-record(state, {
	next :: any(),
    request :: map()
}).

-type state() :: #state{}.

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
	-> {cowboy_stream:commands(), {module(), state()} | undefined}.
init(StreamID, Req, Opts) ->
    {Commands, Next} = cowboy_stream:init(StreamID, Req, Opts),
    {Commands, #state{next = Next, request = Req}}.

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), binary(), {Handler, State} | undefined)
	-> {cowboy_stream:commands(), {Handler, State} | undefined}
	when Handler::module(), State::state().
data(StreamID, IsFin, Data, State) ->
    z:debug_msg(?MODULE, ?LINE, Data),  %% Response body
    cowboy_stream:data(StreamID, IsFin, Data, State).

-spec info(cowboy_stream:streamid(), any(), {Handler, State} | undefined)
	-> {cowboy_stream:commands(), {Handler, State} | undefined}
	when Handler::module(), State::state().
info(StreamID, {response, _, _, _} = Response, #state{request = Request, next = Next}) ->
    z_access_syslog:log_access(Request, Response),
    cowboy_stream:info(StreamID, Response, Next);
info(StreamID, Info, #state{next = Next}) ->
    cowboy_stream:info(StreamID, Info, Next).

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), {module(), state()} | undefined) -> ok.
terminate(StreamID, Reason, State) ->
    cowboy_stream:terminate(StreamID, Reason, State).

