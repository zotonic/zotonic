{% if status == "pending" %}{_ Waiting _}
{% elseif status == "submitting" %}{_ Sending — awaiting confirmation _}
{% elseif status == "queued" %}{_ Queued _}
{% elseif status == "retrying" %}{_ Retrying _}
{% elseif status == "sent" %}{_ Sent _}
{% elseif status == "failed" %}{_ Failed _}
{% elseif status == "bounced" %}{_ Returned by receiving server _}
{% elseif status == "skipped" %}{_ Skipped _}
{% elseif status == "cancelled" %}{_ Not sent — stopped _}
{% else %}{{ status|escape }}{% endif %}
