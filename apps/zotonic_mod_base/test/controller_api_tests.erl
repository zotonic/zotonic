-module(controller_api_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

head_allowed_test() ->
    {Methods, _} = controller_api:allowed_methods(#context{}),
    ?assert(lists:member(<<"HEAD">>, Methods)).

head_model_get_test() ->
    ?assertNot(is_malformed(<<"HEAD">>, <<"model/zotonic_status/get/check">>, <<>>)),
    ?assertNot(is_malformed(<<"HEAD">>, <<"model/example/get/value">>, <<"foo=bar">>)).

head_other_topics_test_() ->
    [
        ?_assert(is_malformed(<<"HEAD">>, Topic, <<>>))
        || Topic <- [
            <<"model/example/post/value">>,
            <<"model/example/delete/value">>,
            <<"model/example">>,
            <<"example/get/value">>
        ]
    ].

head_response_topic_test_() ->
    [
        ?_assert(is_malformed(<<"HEAD">>, <<"model/example/get/value">>, Query))
        || Query <- [
            <<"response_topic=reply/topic">>,
            <<"response_topic=">>,
            <<"response_topic">>,
            <<"response%5Ftopic=reply/topic">>,
            <<"foo=bar&response_topic=reply/topic">>
        ]
    ].

get_response_topic_test() ->
    ?assertNot(is_malformed(<<"GET">>, <<"model/example/get/value">>, <<"response_topic=reply/topic">>)).

is_malformed(Method, Topic, Query) ->
    % malformed_request runs before the middleware populates the context's query arguments.
    Context = #context{
        cowreq = #{ method => Method, qs => Query },
        cowenv = #{ cowmachine_disp_path => Topic }
    },
    {IsMalformed, _} = controller_api:malformed_request(Context),
    IsMalformed.
