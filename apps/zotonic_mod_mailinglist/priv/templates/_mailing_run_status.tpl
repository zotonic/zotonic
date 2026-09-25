{% if status == "scheduled" %}
    {% if type == "publication" %}{_ Waiting for publication _}
    {% elseif due and not due|in_future %}{_ Queued _}
    {% else %}{_ Scheduled _}
    {% endif %}
{% elseif status == "preparing" %}{_ Preparing recipients _}
{% elseif status == "sending" %}{_ Sending _}
{% elseif status == "retrying" %}{_ Retrying _}
{% elseif status == "completed" %}{_ Finished _}
{% elseif status == "completed_errors" %}{_ Finished — some emails could not be sent _}
{% elseif status == "cancelled" %}{_ Sending stopped _}
{% elseif status == "interrupted" %}{_ Sending needs attention _}
{% elseif status == "empty" %}{_ Nothing sent _}
{% elseif status == "failed" %}{_ Failed _}
{% else %}{{ status|escape }}
{% endif %}
