-module(m_rsc_get_raw_db_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/zotonic.hrl").
-export([convert/3, computed/3]).

raw_conversion_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    HasPropsJSON = lists:member(props_json, z_db:column_names(rsc, Context)),
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => text}, Context),
    try
        % The test sandbox can still use the legacy props-only schema.
        SeedSQL = case HasPropsJSON of
            true -> "update rsc set props = $2, props_json = null where id = $1";
            false -> "update rsc set props = $2 where id = $1"
        end,
        z_db:q(SeedSQL,
            [Id, ?DB_PROPS(#{<<"raw_conversion_test">> => {legacy, 42}})], Context),
        z_depcache:flush(Id, Context),
        z_notifier:observe(rsc_get_raw, {?MODULE, convert}, Context),
        z_notifier:observe(rsc_get, {?MODULE, computed}, Context),
        {ok, Raw} = m_rsc:get_raw(Id, Context),
        ?assertEqual(#{<<"value">> => 42}, maps:get(<<"raw_conversion_test">>, Raw)),
        ?assertNot(maps:is_key(<<"raw_computed_test">>, Raw)),
        {ok, Locked} = z_db:transaction(fun(Ctx) -> m_rsc:get_raw_lock(Id, Ctx) end, Context),
        ?assertEqual(Raw, Locked),
        ?assertEqual(true, m_rsc:p_no_acl(Id, <<"raw_computed_test">>, Context)),
        % An unrelated edit must persist the converted old custom property.
        {ok, Id} = m_rsc:update(Id, #{<<"title">> => <<"Changed">>}, Context),
        {ok, Stored} = z_db:qmap_props_row("select * from rsc where id = $1", [Id], Context),
        ?assertEqual(#{<<"value">> => 42}, maps:get(<<"raw_conversion_test">>, Stored)),
        ?assertNot(maps:is_key(<<"raw_computed_test">>, Stored)),
        case HasPropsJSON of
            true ->
                % Only JSON-capable schemas migrate away from the props column.
                ?assertEqual(undefined, z_db:q1("select props from rsc where id = $1", [Id], Context));
            false ->
                ok
        end
    after
        z_notifier:detach(rsc_get_raw, Context),
        z_notifier:detach(rsc_get, Context),
        m_rsc:delete(Id, Context)
    end.

convert(#rsc_get_raw{is_props_only = IsPropsOnly}, Props, _Context) ->
    ?assertEqual(not IsPropsOnly, maps:is_key(<<"id">>, Props)),
    convert_props(Props).

convert_props(#{<<"raw_conversion_test">> := {legacy, Value}} = Props) ->
    Props#{<<"raw_conversion_test">> => #{<<"value">> => Value}};
convert_props(Props) ->
    Props.

computed(#rsc_get{}, #{<<"raw_conversion_test">> := _} = Props, _Context) ->
    Props#{<<"raw_computed_test">> => true};
computed(#rsc_get{}, Props, _Context) ->
    Props.
