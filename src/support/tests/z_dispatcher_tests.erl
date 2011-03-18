%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @hidden

-module(z_dispatcher_tests).

-include_lib("zotonic.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(assertEqualFlat(A, B), ?assertEqual(lists:flatten(A), lists:flatten(B))).

%% test() ->
%%     Ctx  = z_context:new(testsandbox),
%%     List = collect_dispatch_lists(Ctx),
%%     Dict = dispatch_for_uri_lookup(List),
%%     dict:to_list(Dict),

%%     Uri = make_url_for1(
%%                 [{a,"A"},{b,"B"},{c,"hello&plop"},{d,"bla"}], 
%%                 [
%%                 {1, [a],   [a,"a"]},
%%                 {2, [a,b], [a,"ab",b]}, 
%%                 {2, [b,a], [b,"ba",a]} 
%%                 ], 
%%                 html,
%%                 undefined),
%%     lists:flatten(Uri).


url_for_test() ->
    C = z_context:new(testsandbox),
    ?assertEqual(undefined, z_dispatcher:url_for(nonexisting_dispatch_rule, C)),
    %% these dispatch rules come from mod_base.
    ?assertEqualFlat("/", z_dispatcher:url_for(home, C)),
    ?assertEqualFlat("/page/1", z_dispatcher:url_for(page, [{id,1}], C)),
    ?assertEqualFlat("/page/1/a-slug", z_dispatcher:url_for(page, [{id,1}, {slug,"a-slug"}], C)).

