% Basic webmachine resource, copied from webmachine.

-export([ping/2]).

ping(ReqData, State) ->
    {pong, ReqData, State}.

