-module(erlydtl_tests_init).

-export([init/0]).

init() ->
    z_trans_server:start_tests(),
    z_notifier:start_tests(),
    z_ids:start_tests().
