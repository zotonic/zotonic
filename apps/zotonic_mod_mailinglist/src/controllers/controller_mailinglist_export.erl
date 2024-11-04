%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Export the list of all active recipients of a mailinglist.
%% @end

%% Copyright 2009-2023 Marc Worrell
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
    service_available/1,
	forbidden/1,
	allowed_methods/1,
	content_types_provided/1,

	process/4
]).


service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

forbidden(Context) ->
    Context2 = z_context:ensure_qs(Context),
    z_context:logger_md(Context2),
	Id = m_rsc:rid(z_context:get_q(<<"id">>, Context2), Context2),
	{not z_acl:rsc_editable(Id, Context2), Context2}.

allowed_methods(Context) ->
    {[<<"GET">>, <<"HEAD">>], Context}.

content_types_provided(Context) ->
    { [ {<<"text">>, <<"csv">>, []} ], Context }.

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
	Id = m_rsc:rid(z_context:get_q(<<"id">>, Context), Context),
	%% Fetch all exported email addresses
	Recipients = z_mailinglist_recipients:list_recipients(Id, Context),
    Lines = maps:fold(
        fun
            (_Email, RcptId, Acc) when is_integer(RcptId) ->
                case recipient_line(RcptId, Context) of
                    {true, Line} ->
                        [ Line | Acc ];
                    false ->
                        Acc
                end;
            (Email, #{ <<"is_enabled">> := true } = R, Acc) ->
                Line = [
                    Email,
                    maps:get(<<"name_first">>, R, <<>>),
                    maps:get(<<"name_surname">>, R, <<>>),
                    maps:get(<<"name_surname_prefix">>, R, <<>>),
                    maps:get(<<"pref_language">>, R, <<>>),
                    undefined
                ],
                [ Line | Acc ];
            (_Email, _R, Acc) ->
                Acc
        end,
        [],
        Recipients),
    % Add header, encode lines and sort on email address
    Lines1 = [
        [
            <<"Email">>,
            <<"First">>,
            <<"Surname">>,
            <<"Prefix">>,
            <<"Language">>,
            <<"Id">>
        ]
        | Lines
    ],
    Export = lists:sort([ z_csv_writer:encode_line(Line, 9) || Line <- Lines1 ]),
	% Set the content disposition filename
	Filename = <<"mailinglist-", (z_string:to_slug(m_rsc:p(Id, <<"title">>, Context)))/binary, ".csv">>,
	Context1 = z_context:set_resp_header(
                <<"content-disposition">>,
                <<"attachment; filename=", Filename/binary>>,
                Context),
	{Export, Context1}.


recipient_line(Id, Context) ->
    case m_rsc:p(Id, <<"email_raw">>, Context) of
        undefined ->
            false;
        <<>> ->
            false;
        Email when is_binary(Email) ->
            {true, [
                Email,
                unesc(m_rsc:p(Id, <<"name_first">>, Context)),
                unesc(m_rsc:p(Id, <<"name_surname">>, Context)),
                unesc(m_rsc:p(Id, <<"name_surname_prefix">>, Context)),
                m_rsc:p(Id, <<"pref_language">>, Context),
                integer_to_binary(Id)
            ]};
        _ ->
            false
    end.

unesc(undefined) ->
    <<>>;
unesc(V) ->
    z_html:unescape(z_convert:to_binary(V)).
