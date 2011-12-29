Zynamo -- how handoff is managed
================================

Every service (store) keeps an administration from which the original command can be derived.
So that the command can be re-send later to the correct node.

The service itself needs to store these values, as it should be able to hold/index/delete data temporarily
for the handoff node.

For a storage service the format could be:

```
    {key, handoff-node, put|delete}
```

Other services might need to keep a different kind of administration. Or none at all.

The zynamo handoff manager can ask for a key for a node.

```
    gen_server:call(ServiceId, {handoff_node, Node}).
    {ok, #zynamo_command{}}
    
    % Send the value to the other node
    % Notify the service that the handoff was successful
    gen_server:cast(ServiceId, {handoff_done, #zynamo_command{}}).
    
    % Repeat till {error, not_found} returned for the {handoff, Node} call.
```

The handoff manager will do this a random delay after a node/service came back, and will repeat this till
the service returns {ok, done}.  The random delay is used to prevent an internal denial of service attack
after a node came back up.


Back pressure
-------------

A service should be able to issue some back pressure to prevent too long message queues.

