%% @doc Deprecated defintions.
%%      These will be removed in a future update

%% Record used for transporting data between the user-agent and the server.
%% This was part of the transport system, before the Cotonic/MQTT integration.
%% It is still possible to use in combination with the `z_transport` javascript
%% function.
-record(z_msg_v1, {
    qos = 0 :: 0 | 1 | 2,
    dup = false :: boolean(),
    msg_id :: undefined | binary(),
    timestamp :: undefined | pos_integer(),
    content_type = ubf :: text | javascript | json | form | ubf | atom() | binary(),
    delegate = postback :: postback | mqtt | atom() | binary(),
    push_queue = page :: page | session | user,

    % Set by transports from user-agent to server
    ua_class = undefined :: term() | undefined,  % used to be ua_classifier:device_type()
    session_id :: undefined | binary(),
    page_id :: undefined | binary(),

    % Payload data
    data :: any()
}).

%% Record used for logging site access. It was a system wide notifiction which
%% has now been replaced by a publish on the system topic:
%% $SYS/site/<Site>/log/access. It should no longer be used. The previous
%% notification is no longer available.
%%
%% @doc Access log event for http. Called from the z_stats.
%% Type: notify_sync
-record(http_log_access, {
    timestamp :: erlang:timestamp(),
    status :: undefined | non_neg_integer(),
    status_category :: 'xxx' | '1xx' | '2xx' | '3xx' | '4xx' | '5xx',
    method :: binary(),
    metrics :: map()
}).


