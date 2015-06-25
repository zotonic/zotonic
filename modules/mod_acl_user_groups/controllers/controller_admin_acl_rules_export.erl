
-module(controller_admin_acl_rules_export).

-export([
         init/1,
         service_available/2,
         is_authorized/2,
         content_types_provided/2,
         do_export/2
        ]).

-include_lib("zotonic.hrl").
-include_lib("controller_webmachine_helper.hrl").


init(DispatchArgs) -> 
    {ok, DispatchArgs}.

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
    ?WM_REPLY([{"application/octet-stream", do_export}], Context).

do_export(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    Data = acl_user_groups_export:export(Context),
    Content = erlang:term_to_binary(Data, [compressed]),
    Context1 = set_filename(Context),
    ?WM_REPLY(Content, Context1).

set_filename(Context) ->
    Filename = lists:flatten([
                        "acl-rules-",
                        atom_to_list(z_context:site(Context)),
                        ".dat"]),
    z_context:set_resp_header("Content-Disposition", "attachment; filename="++Filename, Context).
