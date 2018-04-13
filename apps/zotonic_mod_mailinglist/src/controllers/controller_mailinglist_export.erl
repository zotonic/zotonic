%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-12-01
%% @doc Export the list of active recipients of a mailinglist.

%% Copyright 2009 Marc Worrell
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

-module(controller_mailinglist_export).
-author("Marc Worrell <marc@worrell.nl>").

-export([
	forbidden/1,
	allowed_methods/1,
	charsets_provided/1,
	content_types_provided/1,

	to_text_csv/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

forbidden(Context) ->
    Context2 = z_context:ensure_qs(Context),
    z_context:lager_md(Context2),
	Id = m_rsc:rid(z_context:get_q(<<"id">>, Context2), Context2),
	{not z_acl:rsc_editable(Id, Context2), Context2}.

allowed_methods(Context) ->
    {[<<"GET">>, <<"HEAD">>], Context}.

charsets_provided(Context) ->
    {[<<"utf-8">>], Context}.

content_types_provided(Context) ->
    { [{<<"text/csv">>, to_text_csv}], Context }.

to_text_csv(Context) ->
	Id = m_rsc:rid(z_context:get_q(<<"id">>, Context), Context),
	%% Fetch all exported email addresses
	Recipients = m_mailinglist:get_enabled_recipients(Id, Context),
	Export = z_utils:combine([13,10], Recipients),
	%% Set the content disposition filename
	Filename = <<"mailinglist-", (z_string:to_slug(m_rsc:p(Id, title, Context)))/binary, ".csv">>,
	Context1 = z_context:set_resp_header(
                <<"content-disposition">>,
                <<"attachment; filename=", Filename/binary>>,
                Context),
	{Export, Context1}.
