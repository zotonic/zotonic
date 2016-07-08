Transport
=========

.. _ref-transport:

The message is defined as::

    -record(z_msg_v1, {
        qos = 0 :: 0 | 1 | 2,
        dup = false :: boolean(),
        msg_id :: binary(),
        timestamp :: pos_integer(),
        content_type = ubf :: text | javascript | json | form | ubf | atom() | binary(),
        delegate = postback :: postback | mqtt | atom() | binary(),
        push_queue = page :: page | session | user,

        % Set by transports from user-agent to server
        session_id :: binary(),
        page_id :: binary(),

        % Payload data
        data :: any()
    }).

The ack message is defined as::

    -record(z_msg_ack, {
        qos = 1 :: 1 | 2,
        msg_id :: binary(),
        push_queue = page :: page | session | user,
        session_id :: binary(),
        page_id :: binary(),
        result :: any()
    }).
