.. _cookbook-task-queue:

Execute tasks asynchronously using the task queue
=================================================

The Zotonic task queue lets applications perform tasks asynchronously.

Let’s say you have some external HTTP API that you want to update whenever
a resource in Zotonic is changed. You can so by queuing an task after each
resource update. The HTTP request to the external API will be executed
asynchronously, so you application and its users do not have have to wait for
it.

Add a task to the queue
-----------------------

To add a task to the queue, provide a module and function that should be called
when the task is popped from the queue. So to add a task that will call the
``external_api_client:update_external_rsc()`` as a callback function::

    z_pivot_rsc:insert_task(
        external_api_client,
        update_external_rsc,
        Context
    ).

You can also supply arguments that will be passed to the function. So to have
``external_api_client:update_external_rsc(RscId)`` called::

    RscId = 123,
    z_pivot_rsc:insert_task(
        external_api_client,
        update_external_rsc,
        undefined,
        [RscId]
        Context
    ).

If you want to queue the task whenever a resource is changed, add this code
to an :ref:`rsc_update_done` observer in your :ref:`site module <module-file>`::

    %% yoursite.erl
    module(yoursite).

    -export([
        observe_rsc_update_done/2
    ]).

    observe_rsc_update_done(#rsc_update_done{id = RscId}, Context) ->
        z_pivot_rsc:insert_task(
            external_api_client,
            update_external_rsc,
            undefined,
            [RscId]
            Context
        ).

Execute queued tasks
--------------------

Add the callback function ``update_external_rsc/2`` referenced above to your module::

    %% external_api_client.erl
    -module(external_api_client).

    -export([
    	update_external_rsc/2
    ]).

    update_external_rsc(RscId, Context) ->
        %% Fetch resource properties
        Json = z_convert:to_json(m_rsc_export:full(Id, Context)),

        %% Execute HTTP query to external API
        {ok, Response} = httpc:request(
            post,
            {
                "https://some-external-api.com",
                [],
                "application/json",
                Json
            },
            [],
            []
        ),

        %% Return anything to signal the task was executed successfully
        ok.

.. note::

    Your callback function receives an anonymous ``Context``, so if it performs actions that
    depend on access checks, make sure to use either ``m_rsc:p_no_acl/3`` or
    ``z_acl:sudo(Context)``.

Handle failing tasks
--------------------

The ``update_external_rsc`` function above assumes that the HTTP request will
return successfully. Of course, this is not always the case. To handle failing
tasks, you can return a ``{delay, NumberOfSeconds}`` tuple that will retry the
task later::

    update_external_rsc(RscId, Context) ->

        case httpc:request(
            ...
        ) of
            {ok, Response} ->
                ok;
            {error, Error} ->
                %% Try the task again in one minute
                {delay, 60}
        end.

Prevent duplicate tasks
-----------------------

We decided above that the task should run whenever a resource is changed in
Zotonic. However, if a resource is quickly edited multiple times in a row, we
only need to send the latest changes once to the external API. In other words,
we want to coalesce the tasks into one. You can do so by providing a unique key
when queueing the task::

    UniqueKey = "external-api-" ++ z_convert:to_list(RscId),
    z_pivot_rsc:insert_task(
        external_api_client,
        update_external_rsc,
        UniqueKey,
        [RscId],
        Context
    ).
