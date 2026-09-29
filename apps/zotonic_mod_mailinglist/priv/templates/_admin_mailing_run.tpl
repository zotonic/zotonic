{% if m.mailinglist_run.run[run_id] as run %}
<header class="admin-header mailing-run-heading">
    {% if run.is_test %}<span class="label label-info">{_ Test email _}</span>{% endif %}
    <h2>{{ m.rsc[run.page_id].title }}</h2>
    <p class="text-muted">{_ Mailing list _} · <a href="{% url admin_edit_rsc id=run.mailinglist_id %}">{{ m.rsc[run.mailinglist_id].title }}</a></p>
</header>
    {% if run.details_expired %}<p class="alert alert-info">{_ Recipient details and test addresses have expired after three months. Totals are retained as a snapshot; delivery updates and recipient retries are no longer available. Start a new mailing to send this page again. _}</p>{% endif %}

<section class="widget mailing-run-section" aria-labelledby="mailing-overview-title">
    <div class="widget-header mailing-run-overview-heading">
        <h3 id="mailing-overview-title">{_ Overview _}</h3>
        <span class="label {% if run.status == "completed" %}label-success{% elseif run.status == "failed" or run.status == "interrupted" %}label-danger{% elseif run.status == "completed_errors" or run.status == "empty" or run.status == "cancelled" %}label-warning{% else %}label-info{% endif %}" aria-live="polite">{% include "_mailing_run_status.tpl" status=run.status type=run.type due=run.due %}</span>
    </div>
    <div class="widget-content">
    {% if run.test_address %}
        <p>{_ Test recipient: _} <strong>{{ run.test_address|escape }}</strong></p>
    {% endif %}
    <p>
        <span class="text-muted">{_ Language: _}</span>
        <strong>{% if run.language %}{{ m.translation.language_list_configured[run.language].name|default:run.language|escape }}{% else %}{_ Recipient preference _}{% endif %}</strong>
        {% if not run.language and run.language_policy %}
            <br><span class="text-muted">{_ Language selection: _}</span>
            <strong>{% if run.language_policy == "all" %}{_ Everyone, using their best available language _}{% else %}{_ Matching preferred languages only _}{% endif %}</strong>
        {% endif %}
        {% if run.language_policy != "matching" or run.language %}
        <br><span class="text-muted">{% if run.language_policy == "all" %}{_ Fallback language: _}{% else %}{_ Language when no preference is known: _}{% endif %}</span>
        <strong>{{ m.translation.language_list_configured[run.fallback_language].name|default:run.fallback_language|escape }}</strong>
        {% endif %}
        <br><span class="text-muted">{_ Sender: _}</span>
        <strong>
            {% if run.sender_id %}
                <a href="{% url admin_edit_rsc id=run.sender_id %}">{% include "_name.tpl" id=run.sender_id %}</a>
            {% else %}
                <em>{_ Unknown _}</em>
            {% endif %}
        </strong>
    </p>
    {% if run.parent_id %}
        <p>
            {_ This is a resend of an earlier mailing. _}
            <a href="{% url admin_mailing_run run_id=run.parent_id %}">{_ View original mailing _}</a>
        </p>
    {% endif %}

    {% include "_mailing_run_summary.tpl" stats=run.stats %}

    {% if run.status == "empty" %}
        <p class="alert alert-warning">
            {_ Nobody received this mailing. Check the recipient below for the reasons, then change the language or recipient selection. _}
        </p>
    {% endif %}
    {% if run.status == "cancelled" %}
        <p>
            {_ Sending stopped. _} {{ run.stats.sent|default:0 }} {_ sent; _}
            {{ run.stats.queued|default:0 }} {_ already queued; _}
            {{ run.stats.retrying|default:0 }} {_ still being retried; _}
            {{ run.stats.submitting|default:0 }} {_ awaiting confirmation; _}
            {{ run.stats.cancelled|default:0 }} {_ will not be sent. _}
        </p>
    {% endif %}
    </div>
</section>

<div class="mailing-run-layout">
    <section class="widget mailing-run-section" aria-labelledby="mailing-phases-title">
        <div class="widget-header"><h3 id="mailing-phases-title">{_ Mailing phases _}</h3></div>
        <div class="widget-content">{% include "_mailing_run_timeline.tpl" run=run %}</div>
    </section>

    <div>
        <section class="widget mailing-run-section" aria-labelledby="mailing-content-title">
            <div class="widget-header"><h3 id="mailing-content-title">{_ Saved mailing content _}</h3></div>
            <div class="widget-content">
                {% if run.copies %}
                    <p>{_ Language copies saved when sending started, before recipient-specific personalization. _}</p>
                    <ul class="mailing-run-copies">
                        {% for copy in run.copies %}
                            <li>
                                <a target="_blank" rel="noopener" title="{_ Opens in a new tab _}" href="{% url admin_mailing_run_content run_id=run.id language=copy.language %}">{{ m.translation.language_list_configured[copy.language].name|default:copy.language|escape }} <span aria-hidden="true">↗</span></a>
                                <br><small class="text-muted">{{ copy.created|date:"Y-m-d H:i:s" }}</small>
                            </li>
                        {% endfor %}
                    </ul>
                {% elseif not run.started %}
                    <p>{_ Copies will be saved in the selected languages when sending starts. _}</p>
                {% else %}
                    <p>{_ No saved copies are available. Older mailings and mailings without selected recipients may have no copies. _}</p>
                {% endif %}
            </div>
        </section>
        <section class="widget mailing-run-section" aria-labelledby="mailing-languages-title">
            <div class="widget-header"><h3 id="mailing-languages-title">{_ Results by language _}</h3></div>
            <div class="widget-content table-responsive">
                <table class="table table-condensed mailing-run-languages">
                    <thead>
                        <tr><th>{_ Language _}</th><th>{_ Result _}</th><th>{_ Recipients _}</th></tr>
                    </thead>
                    <tbody>
                    {% for row in run.languages %}
                        <tr>
                            <td>{{ m.translation.language_list_configured[row.language].name|default:row.language|escape }}</td>
                            <td>{% include "_mailing_recipient_status.tpl" status=row.status %}</td><td>{{ row.total }}</td>
                        </tr>
                    {% empty %}
                        <tr>
                            <td colspan="3" class="text-muted">{_ Results will appear when sending starts. _}</td>
                        </tr>
                    {% endfor %}
                    </tbody>
                </table>
            </div>
        </section>
    </div>
</div>

<section class="widget mailing-run-section" aria-labelledby="mailing-actions-title">
    <div class="widget-header"><h3 id="mailing-actions-title">{_ Mailing actions _}</h3></div>
    <div class="widget-content">
        <div class="mailing-run-actions">
            <a class="btn btn-default" href="{% url admin_edit_rsc id=run.page_id %}">{_ Edit page _}</a>
            <a class="btn btn-default" href="{% url admin_mailing_status id=run.page_id %}">{_ Send another mailing _}</a>
            {% if run.status == "scheduled"
                  or run.status == "preparing"
                  or run.status == "sending"
                  or run.status == "retrying"
                  or run.status == "interrupted"
            %}
                {% if run.stats.pending or not run.prepared and not run.stats.total %}
                    {% button class="btn btn-danger"
                              text=_"Stop sending"
                              action={confirm
                                    text=_"Stop sending? Only emails not yet queued can be stopped. Emails already queued will still be sent."
                                    is_danger
                                    ok=_"Stop sending"
                                    postback={mailing_run_cancel run_id=run.id}
                                    delegate="mod_mailinglist"
                              }
                    %}
                {% endif %}
            {% endif %}
            {% if run.status == "interrupted" %}
                {% if run.stats.pending or not run.prepared and not run.stats.total %}
                    {% button class="btn btn-default"
                              text=_"Continue sending"
                              postback={mailing_run_resume run_id=run.id}
                              delegate="mod_mailinglist"
                    %}
                {% endif %}
                <p>{{ run.stats.pending|default:0 }} {_ emails are waiting to be sent. _}</p>
                {% if run.stats.submitting %}
                    <p class="alert alert-warning">{_ Some emails have an uncertain sending result. Ask your administrator to investigate before sending them again, to avoid duplicates. _}</p>
                {% endif %}
            {% endif %}
            {% if not run.details_expired %}
                {% if run.status == "completed" or run.status == "completed_errors" or run.status == "failed" or run.status == "cancelled" or run.status == "empty" %}
                    {% if run.is_test %}
                        {% button class="btn btn-default" text=_"Send test again" postback={mailing_resend_review run_id=run.id mode="all"} delegate="action_mailinglist_dialog_mailing_page" %}
                    {% else %}
                        {% with m.mailinglist_run.history_expired[run.page_id][run.mailinglist_id] as history_expired %}
                            {% if not history_expired %}
                                {% if run.stats.failed or run.stats.bounced %}
                                    {% button class="btn btn-default"
                                              text=_"Retry failed recipients"
                                              postback={mailing_resend_review run_id=run.id mode="failed"}
                                              delegate="action_mailinglist_dialog_mailing_page" %}
                                {% endif %}
                                {% button class="btn btn-default"
                                          text=_"Send to people who have not received this page"
                                          postback={mailing_resend_review run_id=run.id mode="new"}
                                          delegate="action_mailinglist_dialog_mailing_page"
                                %}
                            {% endif %}
                        {% endwith %}
                        {% button class="btn btn-default"
                                  text=_"Send again to everyone"
                                  postback={mailing_resend_review run_id=run.id mode="all"}
                                  delegate="action_mailinglist_dialog_mailing_page"
                        %}
                    {% endif %}
                {% endif %}
            {% endif %}
        </div>
        {% if run.error %}
            <details class="mailing-run-diagnostics">
                <summary>{_ Technical details _}</summary>
                {{ run.error|escape }}
            </details>
        {% endif %}
    </div>
</section>
{% endif %}
