%% @author Arjan Scherpenisse
%% @copyright 2022 Arjan Scherpenisse
%% @doc Export the ACL rules in a format that can be imported again.

%% Copyright 2022 Arjan Scherpenisse
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(controller_admin_acl_rules_export).

-export([
        service_available/1,
        is_authorized/1,
        content_types_provided/1,
        process/4
    ]).

service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

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
