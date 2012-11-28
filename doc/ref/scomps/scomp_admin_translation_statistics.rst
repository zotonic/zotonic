
.. include:: meta-admin_translation_statistics.rst

Shows statistics for a module/language combination about how many
translation strings have been translated into the given language::

  {% admin_translation_statistics module=`mod_translation` lang=`nl` %}

Renders something like::

  <td class="perc-60"><span>120 / 221</span></td>

To indicate that 120 out of 221 strings in :ref:`mod_translation` have
been translated into dutch.
