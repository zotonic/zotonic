%% Query limits are selected from the actual encoder, before loading resources.
-module(export_encoder_limits_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

limits_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(check(Encoder, User, Config, Requested, Expected))
        || {Encoder, Key, Default} <- formats(),
           User <- [undefined, 42],
           {Config, Requested, Expected} <- cases(Key, case {Key, User} of
               {query_max_rows_non_streaming, 42} -> 20000;
               _ -> Default
           end)
    ] end}.

formats() ->
    [{Encoder, query_max_rows_streaming, 50000} || Encoder <- [
        export_encoder_csv, export_encoder_json, export_encoder_atom,
        export_encoder_rss, export_encoder_jsonfeed, export_encoder_ics,
        export_encoder_ubf]] ++
    [{Encoder, query_max_rows_non_streaming, 1000} || Encoder <- [
        export_encoder_xlsx, export_encoder_bert]].

cases(Key, Default) ->
    [
        {#{}, undefined, Default},
        {#{}, <<"999999">>, Default},
        {#{}, <<"12">>, 12},
        {#{}, <<"invalid">>, Default},
        {#{Key => <<"250">>}, <<"999999">>, 250},
        {#{Key => "300"}, undefined, 300},
        {#{Key => 42}, undefined, 42},
        {#{Key => <<"invalid">>}, undefined, Default},
        {#{Key => 0}, undefined, Default},
        {#{Key => -1}, undefined, Default},
        {#{query_max_rows_streaming => 90000, query_max_rows_non_streaming => 60}, undefined,
            case Key of query_max_rows_streaming -> 90000; _ -> 60 end}
    ].

setup() ->
    Encoders = [E || {E, _, _} <- formats()],
    meck:new([m_config, z_search, z_notifier], [no_link]),
    meck:expect(m_config, get_value, fun(mod_export, Key, C) ->
        maps:get(Key, z_context:get(test_config, C), undefined)
    end),
    meck:expect(z_notifier, first, fun(_, _) -> undefined end),
    meck:expect(z_search, search, fun(<<"query">>, _, 1, Limit, _, _) ->
        self() ! {selected_limit, Limit},
        #search_result{result=[]}
    end),
    %% Keep the real MIME selection, while isolating workbook/resource rendering.
    lists:foreach(fun(E) ->
        meck:new(E, [passthrough, no_link]),
        meck:expect(E, init, fun(_, _) -> {ok, undefined} end),
        meck:expect(E, header, fun(_, State, _) -> {ok, <<>>, State} end),
        meck:expect(E, footer, fun(_, _, _) -> {ok, <<>>} end)
    end, Encoders),
    Encoders.

cleanup(Encoders) -> meck:unload(Encoders ++ [m_config, z_search, z_notifier]).

check(Encoder, User, Config, Requested, Expected) ->
    Args = case Requested of undefined -> []; _ -> [{<<"pagelen">>, Requested}] end,
    C = z_context:set(test_config, Config, z_context:set_q_all(Args, #context{user_id=User})),
    {stream, Stream} = export_encoder:stream(1, hd(Encoder:mime()), [{is_query, true}], C),
    %% Query execution must remain lazy for both encoder classes.
    receive {selected_limit, _} -> error(eager_query) after 0 -> ok end,
    consume(Stream),
    receive {selected_limit, Limit} -> ?assertEqual(Expected, Limit)
    after 1000 -> error(missing_query) end.

consume({_, done}) -> ok;
consume({_, Next}) -> consume(Next()).
