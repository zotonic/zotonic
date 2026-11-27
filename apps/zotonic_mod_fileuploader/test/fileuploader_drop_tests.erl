%% Regression checks for contextual admin uploads. No site or uploaded files are needed.
-module(fileuploader_drop_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

drop_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun rejects_multiple_replacements/0,
        fun processes_selection_order/0
    ]}.

setup() ->
    Modules = [z_fileuploader, z_render, z_mqtt, z_trans],
    lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
    meck:expect(z_fileuploader, stop, fun(_) -> ok end),
    meck:expect(z_fileuploader, status, fun(_) -> {error, enoent} end),
    meck:expect(z_render, growl_error, fun(_, C) -> C end),
    meck:expect(z_render, wire, fun(_, C) -> C end),
    meck:expect(z_mqtt, publish, fun(_, _, _) -> ok end),
    meck:expect(z_trans, trans, fun(Text, _) -> Text end),
    Modules.

cleanup(Modules) ->
    lists:foreach(fun meck:unload/1, Modules).

rejects_multiple_replacements() ->
    Result = mod_fileuploader:event(event(#{<<"id">> => 123, <<"is_medium">> => true}, [
        #{<<"upload">> => <<"one">>}, #{<<"upload">> => <<"two">>}
    ]), #context{}),
    ?assertEqual(true, z_context:get(fileuploader_failed, Result)),
    ?assertEqual(0, meck:num_calls(z_fileuploader, status, '_')),
    ?assertEqual(2, meck:num_calls(z_fileuploader, stop, '_')),
    ?assert(meck:called(z_render, wire, [{unmask, [{target, <<"zone">>}]}, '_'])).

processes_selection_order() ->
    mod_fileuploader:event(event(#{}, [
        #{<<"name">> => <<"0000000001">>, <<"upload">> => <<"second">>},
        #{<<"name">> => <<"0000000000">>, <<"upload">> => <<"first">>}
    ]), #context{}),
    Names = [Name || {_, {z_fileuploader, status, [Name]}, _} <- meck:history(z_fileuploader)],
    ?assertEqual([<<"first">>, <<"second">>], Names).

event(Data, Files) ->
    #z_msg_v1{data = [{<<"data">>, Data}, {<<"target">>, <<"zone">>}, {<<"fileuploader">>, Files}]}.


validation_test_() ->
    {setup,
        fun() ->
            lists:foreach(fun(M) -> meck:new(M, [no_link]) end, [z_notifier, z_media_identify, z_acl]),
            meck:expect(z_notifier, first, fun(#acl_max_upload_size{}, _) -> 100 end),
            meck:expect(z_media_identify, guess_mime, fun(_) -> <<"text/plain">> end),
            meck:expect(z_acl, is_allowed, fun(insert, #acl_media{}, _) -> false end)
        end,
        fun(_) -> lists:foreach(fun meck:unload/1, [z_notifier, z_media_identify, z_acl]) end,
        fun(_) -> [
            ?_assertEqual({error, file_too_large}, validate(101)),
            ?_assertEqual({error, file_not_allowed}, validate(100)),
            ?_assertEqual({error, missing_fileinfo}, validate(0)),
            ?_test(begin
                meck:expect(z_acl, is_allowed, fun(insert, #acl_media{}, _) -> true end),
                ?assertEqual({ok, true}, validate(100))
            end)
        ] end}.

validate(Size) ->
    m_fileuploader:m_post([<<"validate">>],
        #{payload => #{<<"filename">> => <<"test.txt">>, <<"size">> => Size}}, #context{}).
