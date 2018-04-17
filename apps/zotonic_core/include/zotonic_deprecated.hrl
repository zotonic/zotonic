%% @doc Deprecated defintions.
%%      These will be removed in a future update


%% @doc Handle a javascript notification from the postback handler. The 'message' is the z_msg argument of
%% the request. (first), 'trigger' the id of the element which triggered the postback, and 'target' the
%% id of the element which should receive possible updates. Note: postback_notify is also used as an event.
%% Return: ``undefined`` or ``#context{}`` with the result of the postback
-record(postback_notify, {message, trigger, target, data}).

%% @doc Message sent by a user-agent on a postback event. Encapsulates the encoded postback and any
%% additional data. This is handled by z_transport.erl, which will call the correct event/2 functions.
-record(postback_event, {postback, trigger, target, triggervalue, data}).


%% Record used for transporting data between the user-agent and the server.
%% This was part of the transport system, before the Cotonic/MQTT integration.
-record(z_msg_v1, {
    qos = 0 :: 0 | 1 | 2,
    dup = false :: boolean(),
    msg_id :: binary(),
    timestamp :: pos_integer(),
    content_type = ubf :: text | javascript | json | form | ubf | atom() | binary(),
    delegate = postback :: postback | mqtt | atom() | binary(),
    push_queue = page :: page | session | user,

    % Set by transports from user-agent to server
    ua_class = undefined :: ua_classifier:device_type() | undefined,
    session_id :: binary(),
    page_id :: binary(),

    % Payload data
    data :: any()
}).

%% @doc Postback event received from browser.
%%
-record(postback, {message, trigger, target}).

%% @doc Submit event received from browser.
%%
-record(submit, {message, form, target}).

%% The record of the postback_notify event is defined in zotonic_notifications.hrl

%% Drag and drop events. They are received in a normal postback event by scomp_base_droppable.
%% The are emitted as separate event.
%%

%% [Deprecated] Drag and drop event message -- used by scomps draggable and droppable
-record(dragdrop, {tag, delegate, id}).

%% @doc Drag event.
-record(drag, {drag, drop}).

%% @doc Drop event.
-record(drop, {drag, drop}).

%% Sort event. It is received in a normal postback event by scomp_base_sorter.
%%

%% @doc Sort event.
-record(sort, {items, drop}).

