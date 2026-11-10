-module(z_user_agent_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

cached_crawler_test() ->
    Context = #context{},
    ?assert(z_user_agent:is_crawler(z_context:set(is_crawler,true,Context))),
    ?assertNot(z_user_agent:is_crawler(z_context:set(is_crawler,false,Context))),
    ?assert(z_user_agent:is_crawler(<<"curl/8.0">>)),
    ?assertNot(z_user_agent:is_crawler(<<"Mozilla/5.0 ordinary-browser">>)).
