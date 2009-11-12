% Basic webmachine resource, copied from webmachine.

-export([start_link/1]).
-export([ping/2]).

start_link(Args) ->
    webmachine_resource:start_link(?MODULE, [Args]).

ping(ReqData, State) ->
    {pong, ReqData, State}.

