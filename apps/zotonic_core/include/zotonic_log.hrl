%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2024 Marc Worrell
%% @doc Log record definitions for zotonic
%% @end

%% Copyright 2011-2024 Marc Worrell
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
-define(LOG_LEVEL_NOTICE, 3).
-define(LOG_LEVEL_INFO, 4).
-define(LOG_LEVEL_DEBUG, 5).

-record(log_message, {
    type = error :: z:severity(),
    user_id :: m_rsc:resource_id() | undefined,
    message :: iodata() | undefined,
    props = [] :: proplists:proplist()
}).


-record(log_email, {
    severity = ?LOG_LEVEL_ERROR :: ?LOG_LEVEL_FATAL..?LOG_LEVEL_DEBUG,
    message_nr :: binary() | undefined,
    mailer_status :: sending | sent | error | retry | warning | bounce | received | blocked,
    mailer_message :: binary() | undefined | tuple(),      % any text, to clarify the mailer_status
    mailer_host :: undefined | binary() | string(),        % SMTP server or client we are talking with
    envelop_to :: undefined | binary() | string(),         % the 'to' on the envelop
    envelop_from :: undefined | binary() | string(),       % the 'from' on the envelop
    to_id :: undefined | m_rsc:resource_id(),              % who is receiving the e-mail
    from_id :: undefined | m_rsc:resource_id(),            % who is sending (user in the #context)
    content_id :: undefined | m_rsc:resource_id(),         % The page being sent (if any)
    other_id :: undefined | m_rsc:resource_id(),           % In case of a mailinglist the mailinglist id
    message_template :: undefined | template_compiler:template(),  % template used for rendering the e-mail (if any)
    props = [] :: proplists:proplist()                     % optional extra properties to be logged
}).

% NOTE: Make sure to extend record_to_proplist/1 in mod_logging.erl when adding log types.
-record(zlog, {
    type = debug :: z:severity() | atom(),
    user_id = undefined :: m_rsc:resource_id() | undefined,
    timestamp = undefined :: erlang:timestamp() | undefined,
    props = [] :: proplists:proplist() | #log_message{} | #log_email{}
}).


-define(zLoc, #{mfa=>{?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY},
                line=>?LINE,
                file=>?FILE}).

%% Log notifications
-define(zDebug(Msg, Context), z:debug(Msg, ?zLoc, Context)).
-define(zInfo(Msg, Context), z:info(Msg, ?zLoc, Context)).
-define(zNotice(Msg, Context), z:notice(Msg, ?zLoc, Context)).
-define(zWarning(Msg, Context), z:warning(Msg, ?zLoc, Context)).
-define(zError(Msg, Context), z:error(Msg, ?zLoc, Context)).
-define(zFatal(Msg, Context), z:fatal(Msg, ?zLoc, Context)).

-define(zDebug(Msg, Args, Context), z:debug(Msg, Args, ?zLoc, Context)).
-define(zInfo(Msg, Args, Context), z:info(Msg, Args, ?zLoc, Context)).
-define(zNotice(Msg, Args, Context), z:notice(Msg, Args, ?zLoc, Context)).
-define(zWarning(Msg, Args, Context), z:warning(Msg, Args, ?zLoc, Context)).
-define(zError(Msg, Args, Context), z:error(Msg, Args, ?zLoc, Context)).
-define(zFatal(Msg, Args, Context), z:fatal(Msg, Args, ?zLoc, Context)).

%% Below is copied (and adapted) from Nitrogen, which is copyright 2008-2009 Rusty Klophaus

%% Easy to use macros for debugging/development
-define(PRINT(Var), ?LOG_NOTICE(#{ var => ??Var, value => Var })).
-define(DEBUG(Msg), z:debug_msg(Msg, ?zLoc)).
