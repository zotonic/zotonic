%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Relay received e-mails to an user's email address.

%% Copyright 2011 Marc Worrell
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

-module(mod_email_relay).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Email Relay").
-mod_description("Relay incoming e-mails for known users to their private e-mail address.").
-mod_prio(500).

-export([
    observe_email_received/2
]).

-include_lib("zotonic.hrl").

% @doc Check if the recipient is a known user, if so redirect the received e-mail as-is to that user.
observe_email_received(#email_received{localpart=Recipient} = Received, Context) ->
    case m_identity:lookup_by_username(Recipient, Context) of
        undefined ->
            undefined;
        Props ->
            case proplists:get_value(is_verified, Props) of
                true ->
                    UserId = proplists:get_value(rsc_id, Props),
                    Email = m_rsc:p_no_acl(UserId, email, Context),
                    case z_utils:is_empty(Email) of
                        true ->
                            undefined;
                        false ->
                            % Relay the e-mail as-is
                            Msg = #email{
                                to = Email,
                                raw = Received#email_received.raw
                            },
                            z_email:send(Msg, Context)
                    end;
                false ->
                    undefined
            end
    end.
