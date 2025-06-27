
.. include:: meta-mod_cron.rst

Provides periodic events and scheduling of regular module specific jobs.

Current this module sends *tick* notifications at periodic intervals.
These ticks can be observed to implement periodic tasks.

Other modules can schedule jobs which should be triggered at specific
intervals

tick
----

For periodic tasks the system has various periodic tick events.
They are named after their interval *s* for seconds, *m* for minutes,
and *h* for hours.

  * tick_1s
  * tick_1m
  * tick_10m
  * tick_15m
  * tick_30m
  * tick_1h
  * tick_2h
  * tick_3h
  * tick_4h
  * tick_6h
  * tick_12h
  * tick_24h

Example
"""""""

Check something every hour::

    -include_lib("kernel/include/logger.hrl").

    observe_tick_1h(tick_1h, Context) ->
        ?LOG_INFO("And another hour has passed..."),
        do_something(Context).

The return value is ignored.

The *tick* observers are called one by one in a separate process. So a slow
handler can delay the other handlers.

Schedule Module Specific Jobs
-----------------------------

When you want to schedule regular jobs, when you want your module to perorm
jobs at regular intervals it is possible to use `mod_cron` to schedule those
jobs.

For example, if you want to send out an automated weekly reminder to your
users you can do this by defining a module specific task in your module.

Example::

    -mod_depends([cron]).
    -mod_cron_jobs([
        {{weekly, mon, {10, 0, 0}}, {?MODULE, send_weekly_reminders, []}}
    ]).

This definition will be read and will cause the function `send_weekly_reminders`
to be called at monday's on 10:00 UTC. The function will be called with a
context of the site.

Example::

    send_weekly_reminders(Context) ->
        Events = get_this_weeks_events(Context),
        send_email_reminders(Events, Context).

This function will then send out reminders of events taking place the coming week.
Its definition will cause it to be called on monday moring 10:00 UTC. Note that
the `Context` parameter will automatically be added to the function call.

The scheduling of events is done by `erlcron`. For online documentation on how to
schedule jobs and their syntax definition, see: https://hexdocs.pm/erlcron/readme.html
