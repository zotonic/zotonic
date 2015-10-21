
.. include:: meta-mask_progress.rst

Sets the progress bar of a :ref:`action-mask`.

The progress bar can be set to a percentage in the range 0…100.
The target of the ``mask_progress`` must be the same as the target of an earlier ``mask`` action.

Example::

   {% wire action={mask_progress target="logon_outer" percent=50} %}

In this example the `logon_outer` progress bar will show as halfway (50%).
