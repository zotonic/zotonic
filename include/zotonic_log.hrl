%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Log record definitions for zotonic

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

-define(LOG_FATAL, 0).
-define(LOG_ERROR, 1).
-define(LOG_WARNING, 2).
-define(LOG_INFO, 3).
-define(LOG_DEBUG, 4).


-record(log_message, {
    type = error,
    user_id,
    message,
    props = []
}).


-record(log_email, {
    severity = ?LOG_ERROR,
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