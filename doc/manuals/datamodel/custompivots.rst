.. _manual-datamodel-custompivots:

Defining custom pivot columns
=============================

:ref:`manual-datamodel-query-model` can only sort and filter on
:ref:`manual-datamodel-resources` that actually have a database
column. Zotonic's resources are stored in a serialized form. This
allows you to very easily add any property to any resource but
you cannot sort or filter on them until you make database columns
for these properties.

The way to take this on is using the "custom pivot" feature. A custom
pivot table is an extra database table with columns in which the props
you define are copied, so you can filter and sort on them.

Say you want to sort on a property of the resource called `requestor`.

Create (and export!) an ``init/1`` function in your site where you define a custom pivot table::

  init(Context) ->
      z_pivot_rsc:define_custom_pivot(pivotname, [{requestor, "varchar(80)"}], Context),
      ok.

The new table will be called ``pivot_<pivotname>``. To fill the pivot
table with data when a resource gets saved, create a notification
listener function ``observe_custom_pivot/2``::

  observe_custom_pivot({custom_pivot, Id}, Context) ->
      Requestor = m_rsc:p(Id, requestor, Context),
      {pivotname, [{requestor, Requestor}]}.

This will fill the 'requestor' property for every entry in your
database, when the resource is pivoted.

Recompile your site and restart it (so the `init` function is called)
and then in the admin under `System` -> `Status` choose `Rebuild
search indexes`. This will gradually fill the new pivot table. Enable
the logging module and choose "log" in the admin menu to see the pivot
progress. Once the table is filled, you can use the pivot table to do
sorting and filtering.

To sort on 'requestor', do the following::

  {% with m.search.paged[{query custompivot='pivotname' cat='foo' sort='requestor'}] as result %}

Or you can filter on it. The pivot tables are aliassed with a number
in order of their occurrence, with the first pivot table aliassed as
``pivot1``. This allows you to do filtering on custom fields like
this::

  {% with m.search.paged[{query custompivot="pivotname"
                          filter=["pivot1.fieldname", `=`, "hello"]}]
     as result %}

See :ref:`manual-datamodel-query-model` for more details on how to
filter on custom pivot columns.
