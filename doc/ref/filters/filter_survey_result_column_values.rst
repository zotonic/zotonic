
.. include:: meta-survey_result_column_values.rst

.. seealso:: :ref:`survey_result_columns`, :ref:`survey_result_column_values`

Used by the survey module to add extra column values to the result editor.

Example usage::

    {% with id|survey_result_column_values:answer:columns:"html" as vs %}
        {% for col,_ in columns %}
            <td>
                {{ vs[col] }}
            </td>
        {% endfor %}
    {% endwith %}

Where columns has been returned by the ``#survey_result_columns{}`` notification.

