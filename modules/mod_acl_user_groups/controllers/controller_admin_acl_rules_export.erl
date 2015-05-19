
-module(controller_admin_acl_rules_export).

-export([
         init/1,
         service_available/2,
         is_authorized/2,
         content_types_provided/2,
         do_export/2,
         names_to_ids/2
        ]).

-include_lib("zotonic.hrl").
-include_lib("controller_webmachine_helper.hrl").


init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

is_authorized(ReqData, Context) ->
	Context1 = ?WM_REQ(ReqData, Context),
	Context2 = z_context:ensure_all(Context1),
    z_acl:wm_is_authorized([{use, mod_acl_user_groups}], admin_logon, Context2).

content_types_provided(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    ?WM_REPLY([{"text/plain", do_export}], Context).

do_export(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),

    All =
        [{Kind, Type, ids_to_names(m_acl_rule:all_rules(Kind, Type, Context), Context)}
         || {Kind, Type} <-
                [{rsc, edit}, {rsc, publish},
                 {module, edit}, {module, publish}]],

    Content = term_to_binary(All),
    Context1 = set_filename(Context),
    ?WM_REPLY(Content, Context1).


set_filename(Context) ->
    z_context:set_resp_header("Content-Disposition", "attachment; filename=rules.dat", Context).


