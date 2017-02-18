-module(z_cowboy_log_stream_handler).
-author("David de Boer <david@ddeboer.nl>").

-behavior(cowboy_stream).

-export([
    init/3,
    data/4,
    info/3,
    terminate/3
]).

-type state() :: any().

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
	-> {cowboy_stream:commands(), {module(), state()} | undefined}.
init(StreamID, Req, Opts) ->
    z:debug_msg(?MODULE, ?LINE, Req),   %% Map with request headers and properties
    cowboy_stream:init(StreamID, Req, Opts).

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), binary(), {Handler, State} | undefined)
	-> {cowboy_stream:commands(), {Handler, State} | undefined}
	when Handler::module(), State::state().
data(StreamID, IsFin, Data, {Handler, State0}) ->
    z:debug_msg(?MODULE, ?LINE, Data),  %% Response body
    cowboy_stream:data(StreamID, IsFin, Data, {Handler, State0}).

-spec info(cowboy_stream:streamid(), any(), {Handler, State} | undefined)
	-> {cowboy_stream:commands(), {Handler, State} | undefined}
	when Handler::module(), State::state().
info(StreamID, Info, {Handler, State}) ->
    z:debug_msg(?MODULE, ?LINE, Info),  %% Response status code, headers and body
    cowboy_stream:info(StreamID, Info, {Handler, State}).

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), {module(), state()} | undefined) -> ok.
terminate(StreamID, Reason, {Handler, State}) ->
    z:debug_msg(?MODULE, ?LINE, {StreamID, Reason}), %% Termination reason
    cowboy_stream:terminate(StreamID, Reason, {Handler, State}).

