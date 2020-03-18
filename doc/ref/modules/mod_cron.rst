
.. include:: meta-mod_cron.rst

Provides periodic events.

Current this module sends *tick* notifications at periodic intervals.
These ticks can be observed to implement periodic tasks.

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
  * tick_6h
  * tick_12h
  * tick_24h

Example
"""""""

Check something every hour::

    observe_tick_1h(tick_1h, Context) ->
        lager:info("And another hour has passed..."),
        do_something(Context).

The return value is ignored.

The *tick* observers are called one by one in a separate process. So a slow
handler can delay the other handlers.