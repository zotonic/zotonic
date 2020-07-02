%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2020 Marc Worrell
%% @doc Log record definitions for zotonic

%% Copyright 2011-2020 Marc Worrell
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

-include_lib("kernel/include/logger.hrl").

-define(LOG_LEVEL_FATAL, 0).
-define(LOG_LEVEL_ERROR, 1).
-define(LOG_LEVEL_WARNING, 2).
-define(LOG_LEVEL_INFO, 3).
-define(LOG_LEVEL_DEBUG, 4).

-record(log_message, {
    type = error :: z:severity(),
    user_id :: m_rsc:resource_id() | undefined,
    message :: iodata() | undefined,
    props = [] :: proplists:proplist()
}).


-record(log_email, {
    severity = ?LOG_LEVEL_ERROR,
    message_nr,
    mailer_status,      % sending, sent, error, retry, warning, bounce, received
    mailer_message,     % any text, to clarify the mailer_status
    mailer_host,        % SMTP server or client we are talking with
    envelop_to,         % the 'to' on the envelop
    envelop_from,       % the 'from' on the envelop
    to_id,              % who is receiving the e-mail
    from_id,            % who is sending (user in the #context)
    content_id,         % The page being sent (if any)
    other_id,           % In case of a mailinglist the mailinglist id
    message_template,   % template used for rendering the e-mail (if any)
    props = []          % optional extra properties to be logged
}).

% NOTE: Make sure to extend record_to_proplist/1 in mod_logging.erl when adding log types.
-record(zlog, {
    type = debug :: z:severity(),
    user_id = undefined :: m_rsc:resource_id() | undefined,
    timestamp = undefined :: erlang:timestamp() | undefined,
    props = [] :: proplists:proplist() | #log_message{} | #log_email{}
}).


%% Log notifications
-define(zDebug(Msg, Context), z:debug(Msg, [{module, ?MODULE}, {line, ?LINE}], Context)).
-define(zInfo(Msg, Context), z:info(Msg, [{module, ?MODULE}, {line, ?LINE}], Context)).
-define(zWarning(Msg, Context), z:warning(Msg, [{module, ?MODULE}, {line, ?LINE}], Context)).
-define(zError(Msg, Context), z:error(Msg, [{module, ?MODULE}, {line, ?LINE}], Context)).

-define(zDebug(Msg, Args, Context), z:debug(Msg, Args, [{module, ?MODULE}, {line, ?LINE}], Context)).
-define(zInfo(Msg, Args, Context), z:info(Msg, Args, [{module, ?MODULE}, {line, ?LINE}], Context)).
-define(zWarning(Msg, Args, Context), z:warning(Msg, Args, [{module, ?MODULE}, {line, ?LINE}], Context)).
-define(zError(Msg, Args, Context), z:error(Msg, Args, [{module, ?MODULE}, {line, ?LINE}], Context)).


%% Below is copied (and adapted) from Nitrogen, which is copyright 2008-2009 Rusty Klophaus

%% Easy to use macros for debugging/development
-define(PRINT(Var), lager:info("DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-define(DEBUG(Msg), z:debug_msg(?MODULE, ?LINE, Msg)).
