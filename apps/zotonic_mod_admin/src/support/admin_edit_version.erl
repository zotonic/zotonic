%% Copyright 2026 Zotonic. Apache-2.0.
-module(admin_edit_version).
-moduledoc("Version-checked admin saves. Locks the resource while checking the
browser's version and returns the exact saved version, including changes made
by update observers. Callers must not advance a form's version on notifications.").
-export([save/4, reply/4, notify_media_update/2]).
-include_lib("zotonic_core/include/zotonic.hrl").

%% Use the update's exact versions, not a read before/after a slow upload.
%% The private client topic prevents other tabs from acknowledging this change.
-spec notify_media_update(#rsc_update_done{}, z:context()) -> ok.
notify_media_update(#rsc_update_done{action = update, id = Id,
        pre_props = #{<<"version">> := Before}, post_props = #{<<"version">> := After}}, Context) ->
    case {z_context:get(admin_media_update_id, Context), z_context:client_id(Context)} of
        {Id, {ok, _}} ->
            z_mqtt:publish([<<"~client">>, <<"model">>, <<"admin">>, <<"event">>, <<"media-version">>],
                #{id => Id, previous_version => Before, version => After}, Context),
            ok;
        _ -> ok
    end;
notify_media_update(_Update, _Context) ->
    ok.

-spec save(Id, Version, Save, Context) -> Result when
    Id :: m_rsc:resource_id(),
    Version :: term(),
    Save :: fun((z:context()) -> {ok, term()} | {error, term()}),
    Context :: z:context(),
    Result :: {ok, pos_integer()} | {error, term()}.
save(Id, Version, Save, Context) when is_binary(Version) ->
    try binary_to_integer(Version) of
        V -> save(Id, V, Save, Context)
    catch error:badarg -> {error, stale} end;
save(Id, Version, Save, Context) when is_integer(Version), Version > 0 ->
    case z_acl:rsc_editable(Id, Context) of
        false -> {error, eacces};
        true ->
            Result = z_db:transaction(fun(Ctx) ->
                % ACL checked above; raw properties are used only under the row lock.
                case m_rsc:get_raw_lock(Id, Ctx) of
                    {ok, #{ <<"version">> := Version }} ->
                        case Save(Ctx) of
                            {ok, _} ->
                                {ok, z_db:q1("select version from rsc where id = $1", [Id], Ctx)};
                            {error, Reason} -> {rollback, {error, Reason}}
                        end;
                    {ok, _} -> {error, stale};
                    {error, _} = Error -> Error
                end
            end, Context),
            case Result of
                {rollback, Reason} -> {error, Reason};
                _ -> Result
            end
    end;
save(_Id, _Version, _Save, _Context) ->
    {error, stale}.

-spec reply(Form, Id, Result, Context) -> z:context() when
    Form :: binary(), Id :: integer(), Result :: term(), Context :: z:context().
reply(Form, Id, Result, Context) ->
    Version = case Result of {ok, V} -> V; _ -> null end,
    z_render:add_script([
        "window.z_admin_edit_version?.saved(", z_json:encode(Form), ",",
        integer_to_binary(Id), ",", z_json:encode(Version), ");"
    ], Context).
