.. _manual-datamodel-custompivots:

Defining custom pivot columns
=============================

The query model can only sort on things that actually have a database
column. If you make up your own props, they do not have such a column,
so you cannot sort on them.

The way to circumvent this, is using the "custom pivot" feature: this
creates an extra table with columns in which the props you define are
copied.

Say you want to sort on the column "requestor".

In the init function of your site, do the following::

  z_pivot_rsc:define_custom_pivot(pivotname, [{requestor, "varchar(80)"}],
Context),

Then create (and export!) a notification listener function ``observe_custom_pivot/2``::

  observe_custom_pivot({custom_pivot, Id}, Context) ->
      Requestor = m_rsc:p(requestor, Id, Context),
      {pivotname, [{requestor, Requestor}]}.

This will fill the 'requestor' property for every entry in your
database, when the resource is pivoted.

Recompile your module, restart the module (so the `init` function is
called) and then in the admin under `System` -> `Status` choose `Pivot
all`. This will gradually fill the new table. Enable the logging
module and choose "log" in the admin menu to see the pivot progress.

The new table will be called ``pivot_<module>``. Check in the database
to see if its filled.

Now, finally!, you can sort on 'requestor'::

  {% with m.search.paged[{query custompivot='pivotname' cat='foo' sort='requestor'}] as result %}


