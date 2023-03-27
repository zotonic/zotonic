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
    ua_class = undefined :: ua_classifier:device_type() | undefined,
    session_id :: undefined | binary(),
    page_id :: undefined | binary(),

    % Payload data
    data :: any()
}).
