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

-export([
    mailinglist_recipient_key_decode/2
]).

-spec mailinglist_recipient_key_decode(Key, Context) -> Recipient | undefined when
    Key :: binary() | undefined,
    Context :: z:context(),
    Recipient :: m_rsc:resource_id() | Email | undefined,
    Email :: binary().
mailinglist_recipient_key_decode(Key, Context) when is_binary(Key) ->
    case z_mailinglist_recipients:recipient_key_decode(Key, Context) of
        {ok, Recipient} ->
            Recipient;
        {error, _} ->
            undefined
    end;
mailinglist_recipient_key_decode(_Key, _Context) ->
    undefined.
