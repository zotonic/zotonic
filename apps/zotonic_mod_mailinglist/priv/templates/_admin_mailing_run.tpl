{% if m.mailinglist_run.run[run_id] as run %}
<div class="admin-header">
    <h2>{% if run.is_test %}<span class="label label-info">{_ Test email _}</span> {% endif %}{{ m.rsc[run.page_id].title }} → {{ m.rsc[run.mailinglist_id].title }}</h2>
    {% if run.test_address %}<p>{_ Test recipient: _} <strong>{{ run.test_address|escape }}</strong></p>{% endif %}
    <h3 aria-live="polite">{% include "_mailing_run_status.tpl" status=run.status type=run.type due=run.due %}</h3>
    <p>{_ Language: _} {% if run.language %}{{ m.translation.language_list_configured[run.language].name|default:run.language|escape }}{% else %}{_ Recipient preference _}{% endif %}
        · {_ Language when no preference is known: _} {{ m.translation.language_list_configured[run.fallback_language].name|default:run.fallback_language|escape }}
        · {_ Sender: _} {% include "_name.tpl" id=run.sender_id %}</p>
    <p>{_ Created: _} {{ run.created|date:"Y-m-d H:i" }}
        {% if run.started %} · {_ Started: _} {{ run.started|date:"Y-m-d H:i" }}{% endif %}
        {% if run.finished %} · {% if run.status == "cancelled" %}{_ Stopped: _}{% else %}{_ Finished: _}{% endif %} {{ run.finished|date:"Y-m-d H:i" }}{% endif %}</p>
    {% if run.status == "scheduled" and run.type == "date" %}<p>{_ Scheduled for: _} {{ run.due|date:"Y-m-d H:i" }} ({{ m.req.timezone|escape }})</p>{% endif %}
    {% if run.error %}<details><summary>{_ Technical details _}</summary>{{ run.error|escape }}</details>{% endif %}
    {% include "_mailing_run_counts.tpl" stats=run.stats %}
    {% if run.status == "empty" %}<p class="alert alert-warning">{_ Nobody received this mailing. Check the recipient results below for the reasons, then change the language or recipient selection. _}</p>{% endif %}
    {% if run.status == "cancelled" %}<p>{_ Sending stopped. _} {{ run.stats.sent|default:0 }} {_ sent; _}
        {{ run.stats.queued|default:0 }} {_ already queued; _} {{ run.stats.retrying|default:0 }} {_ still being retried; _}
        {{ run.stats.submitting|default:0 }} {_ awaiting confirmation; _} {{ run.stats.cancelled|default:0 }} {_ will not be sent. _}</p>{% endif %}
    <p class="help-block">{_ Sent means accepted by the mail server, not confirmed inbox delivery. Later bounces update these results. _}</p>
    <a class="btn btn-default" href="{% url admin_edit_rsc id=run.page_id %}">{_ Edit page _}</a>
    <a class="btn btn-default" href="{% url admin_mailing_status id=run.page_id %}">{_ Send another mailing _}</a>
    {% if run.status == "scheduled" or run.status == "preparing" or run.status == "sending" or run.status == "retrying" or run.status == "interrupted" %}
        {% if run.stats.pending or not run.prepared and not run.stats.total %}{% button class="btn btn-default" text=_"Stop sending"
            action={confirm text=_"Stop sending? Only emails not yet queued can be stopped. Emails already queued will still be sent."
                postback={mailing_run_cancel run_id=run.id} delegate="mod_mailinglist"} %}{% endif %}
    {% endif %}
    {% if run.status == "interrupted" %}
        {% if run.stats.pending or not run.prepared and not run.stats.total %}{% button class="btn btn-default" text=_"Continue sending"
            postback={mailing_run_resume run_id=run.id} delegate="mod_mailinglist" %}{% endif %}
        <p>{{ run.stats.pending|default:0 }} {_ emails are waiting to be sent. _}</p>
        {% if run.stats.submitting %}<p class="alert alert-warning">{_ Some emails have an uncertain sending result. Ask your administrator to investigate before sending them again, to avoid duplicates. _}</p>{% endif %}
    {% endif %}
    {% if run.status == "completed" or run.status == "completed_errors" or run.status == "failed" or run.status == "cancelled" or run.status == "empty" %}
        {% if run.is_test %}
            {% button class="btn btn-default" text=_"Send test again" postback={mailing_resend_review run_id=run.id mode="all"} delegate="action_mailinglist_dialog_mailing_page" %}
        {% else %}
        {% if run.stats.failed or run.stats.bounced %}{% button class="btn btn-default" text=_"Retry failed recipients"
            postback={mailing_resend_review run_id=run.id mode="failed"} delegate="action_mailinglist_dialog_mailing_page" %}{% endif %}
        {% button class="btn btn-default" text=_"Send to people who have not received this page"
            postback={mailing_resend_review run_id=run.id mode="new"} delegate="action_mailinglist_dialog_mailing_page" %}
        {% button class="btn btn-default" text=_"Send again to everyone"
            postback={mailing_resend_review run_id=run.id mode="all"} delegate="action_mailinglist_dialog_mailing_page" %}
        {% endif %}
    {% endif %}
    {% if run.parent_id %}<p><a href="{% url admin_mailing_run run_id=run.parent_id %}">{_ View original mailing _}</a></p>{% endif %}
    <details><summary>{_ Results by language _}</summary>
        <table class="table"><tbody>{% for row in run.languages %}
            <tr><td>{{ m.translation.language_list_configured[row.language].name|default:row.language|escape }}</td><td>{% include "_mailing_recipient_status.tpl" status=row.status %}</td><td>{{ row.total }}</td></tr>
        {% endfor %}</tbody></table>
    </details>

</div>
{% endif %}
