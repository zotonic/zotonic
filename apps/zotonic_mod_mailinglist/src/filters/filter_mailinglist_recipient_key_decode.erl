%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023 Marc Worrell <marc@worrell.nl>
%% @doc Decode a recipient key to the recipient rsc id or email address.
%% @end

%% Copyright 2023 Marc Worrell
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

-module(filter_mailinglist_recipient_key_decode).
-moduledoc("
Used on the mailing list subscriptions page where users and recipients can unsubscribe from mailinglists.

This filter decodes the passed to the page and extract the user id or the email for which the subscriptions should be shown.
").

-export([
    mailinglist_recipient_key_decode/2
]).

-spec mailinglist_recipient_key_decode(Key, Context) -> Recipient | undefined when
    Key :: binary() | undefined,
    Context :: z:context(),
    Recipient :: map() | undefined.
mailinglist_recipient_key_decode(Key, Context) when is_binary(Key) ->
    case z_mailinglist_recipients:recipient_key_decode(Key, Context) of
        {ok, #{
            recipient := Id,
            list_id := ListId
        }} when is_integer(Id) ->
            Subs = case m_mailinglist:list_subscriptions_by_rsc_id(Id, ListId, Context) of
                {ok, L} ->
                    L;
                {error, _} ->
                    []
            end,
            #{
                <<"rsc_id">> => Id,
                <<"email">> => m_rsc:p_no_acl(Id, <<"email_raw">>, Context),
                <<"is_mailing_opt_out">> => z_convert:to_bool(m_rsc:p_no_acl(Id, <<"is_mailing_opt_out">>, Context)),
                <<"subscriptions">> => Subs
            };
        {ok, #{
            recipient := Email,
            list_id := _ListId
        }} when is_binary(Email) ->
            {ok, Subs} = m_mailinglist:list_subscriptions_by_email(Email, Context),
            #{
                <<"email">> => Email,
                <<"is_mailing_opt_out">> => false,
                <<"subscriptions">> => Subs
            };
        {error, _} ->
            undefined
    end;
mailinglist_recipient_key_decode(_Key, _Context) ->
    undefined.
