-module(webmachine_id).

-export([generate/0]).

generate() ->
    NodeId = 
        case application:get_env(webmachine, node_id) of
            undefined -> 9;
            {ok, NodeId_} -> NodeId_
        end,
    {MegaSec, Sec, MicroSec} = now(),
    io:format("~p  ~p  ~p \n", [MegaSec, Sec, MicroSec]),
    Id = ((NodeId * 1000000 + MegaSec) * 1000000 + Sec) * 1000000 + MicroSec,
    Id.
