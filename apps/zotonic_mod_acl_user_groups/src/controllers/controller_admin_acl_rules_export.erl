
-module(controller_admin_acl_rules_export).

-export([
         is_authorized/1,
         content_types_provided/1,
         process/4
        ]).

-include_lib("zotonic_core/include/zotonic.hrl").

is_authorized(Context) ->
    z_controller_helper:is_authorized([ {use, mod_acl_user_groups} ], Context).

content_types_provided(Context) ->
    {[ {<<"application">>, <<"octet-stream">>, []} ], Context}.

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
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
