%% @hidden

-module(pgsql_pool_tests).

-include_lib("eunit/include/eunit.hrl").

-define(host, "localhost").

get_connections_test() ->
    with_pool_2(
      fun(P) ->
              {ok, C1} = get_connection(P),
              {ok, C2} = get_connection(P),
              ?assert(C1 =/= C2),
              ok = pgsql_pool:return_connection(P, C1),
              ok = pgsql_pool:return_connection(P, C2)
      end).

get_connection_timeout_test() ->
    with_pool_0(
      fun(P) -> 
              {error, timeout} = pgsql_pool:get_connection(P, 100)
      end).

get_returned_connection_test() ->
    with_pool_1(
      fun(P) ->
              {ok, C} = get_connection(P),
              {error, timeout} = get_connection(P),
              ok = pgsql_pool:return_connection(P, C),
              {ok, C} = get_connection(P),    
              ok = pgsql_pool:return_connection(P, C)
      end).

return_twice_test() ->
    with_pool_1(
      fun(P) ->
              {ok, C} = get_connection(P),
              ok = pgsql_pool:return_connection(P, C),
              ok = pgsql_pool:return_connection(P, C)
      end).

connection_dies_test() ->
    with_pool_1(
      fun(P) ->
              {ok, C1} = get_connection(P),
              exit(C1, kill),
              {ok, C2} = get_connection(P),
              ?assert(C1 =/= C2)
      end).

connection_owner_dies_test() ->
    with_pool_1(
      fun(P) ->
              Self = self(),
              spawn(fun() ->
                            {ok, C} = get_connection(P),
                            Self ! {connection, C}
                    end),
              receive
                  {connection, C} ->
                      {ok, C} = get_connection(P),
                      ok = pgsql_pool:return_connection(P, C)
              end
      end).

named_pool_test() ->
    Name = test_pool,
    {ok, _P} = pgsql_pool:start_link(Name, 1, [{host, ?host}]),
    {ok, C} = get_connection(Name),
    ok = pgsql_pool:return_connection(Name, C),
    pgsql_pool:stop(Name).
            
%% -- internal functions --

with_pool(Size, F) ->
    {ok, P} = pgsql_pool:start_link(Size, [{host, ?host}]),
    try F(P)
    after
        pgsql_pool:stop(P)
    end.

with_pool_0(F) -> with_pool(0, F).
with_pool_1(F) -> with_pool(1, F).
with_pool_2(F) -> with_pool(2, F).
    
get_connection(P) ->
    case pgsql_pool:get_connection(P, 1000) of
        {ok, C} ->
            ok = test_connection(C),
            {ok, C};
        Error -> 
            Error
    end.

test_connection(C) ->
    {ok, [_Col], [{<<"1">>}]} = pgsql:squery(C, "select 1"),
    ok.
