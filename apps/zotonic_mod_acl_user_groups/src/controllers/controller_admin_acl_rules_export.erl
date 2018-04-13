
-module(controller_admin_acl_rules_export).

-export([
         is_authorized/1,
         content_types_provided/1,
         do_export/1
        ]).

-include_lib("zotonic_core/include/zotonic.hrl").

is_authorized(Context) ->
    Context2 = z_context:ensure_qs(Context),
    z_acl:wm_is_authorized([{use, mod_acl_user_groups}], admin_logon, Context2).

content_types_provided(Context) ->
    {[{<<"application/octet-stream">>, do_export}], Context}.

do_export(Context) ->
    Data = acl_user_groups_export:export(Context),
    Content = erlang:term_to_binary(Data, [compressed]),
    Context1 = set_filename(Context),
    {Content, Context1}.

set_filename(Context) ->
    Filename = iolist_to_binary([
                        "acl-rules-",
                        atom_to_list(z_context:site(Context)),
                        ".dat"]),
    z_context:set_resp_header(<<"content-disposition">>, <<"attachment; filename=", Filename/binary>>, Context).
