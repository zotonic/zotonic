{% with run.status == "cancelled" or run.status == "failed" or run.status == "empty" as stopped %}
<p class="help-block">{_ Times shown in _} {{ m.req.timezone|escape }}. {_ Future phases have no actual timestamp yet. _}</p>
<div class="table-responsive">
<table class="table table-condensed mailing-run-timeline">
    <thead><tr><th>{_ Phase _}</th><th>{_ Progress _}</th><th>{_ Time _}</th></tr></thead>
    <tbody>
        <tr>
            <th scope="row">1. {_ Mailing confirmed _}</th>
            <td>{_ Done _}</td>
            <td>{{ run.created|date:"Y-m-d H:i:s" }}</td>
        </tr>
        <tr {% if run.status == "scheduled" %}class="info"{% endif %}>
            <th scope="row">2. {_ Wait for sending to start _}</th>
            <td>
                {% if run.status == "scheduled" %}<strong>{_ Current phase _}</strong>
                {% elseif run.started %}{_ Done _}{% else %}{_ Stopped _}{% endif %}
            </td>
            <td>
                {% if run.type == "publication" %}
                    {_ After the page is published _}
                    {% if m.rsc[run.page_id].is_published %}
                        {% if m.rsc[run.page_id].publication_start as publication_start %}
                            <br>{{ publication_start|date:"Y-m-d H:i:s" }} <span class="label label-default">{_ Publication start _}</span>
                        {% endif %}
                    {% endif %}
                {% else %}{{ run.due|date:"Y-m-d H:i:s" }} <span class="label label-default">{_ Planned _}</span>{% endif %}
                {% if run.status == "scheduled" and run.type == "date" and not run.due|in_future %}<br>{_ Waiting for the sending process to start. _}{% endif %}
            </td>
        </tr>
        <tr {% if run.started and not run.prepared and not stopped and run.status != "scheduled" %}class="info"{% endif %}>
            <th scope="row">3. {_ Prepare recipients _}</th>
            <td>
                {% if run.prepared %}{_ Done _}
                {% elseif stopped %}{_ Not completed _}
                {% elseif run.status == "interrupted" %}<strong>{_ Needs attention _}</strong>
                {% elseif run.started and run.status != "scheduled" %}<strong>{_ Current phase _}</strong>
                {% else %}{_ Upcoming _}
                {% endif %}
            </td>
            <td>
                {% if run.started %}{{ run.started|date:"Y-m-d H:i:s" }} <span class="label label-default">{_ Started _}</span>
                {% elseif stopped %}{_ Not started _}
                {% else %}{_ Starts after the waiting phase. _}
                {% endif %}
            </td>
        </tr>
        <tr {% if run.prepared and run.stats.waiting %}class="info"{% endif %}>
            <th scope="row">4. {_ Send emails _}</th>
            <td>
                {% if run.status == "empty" %}{_ Not needed — no recipients selected _}
                {% elseif run.status == "cancelled" %}
                    {% if run.stats.waiting %}{_ Stopped; already queued emails are still being processed _}
                    {% else %}{_ Stopped _}
                    {% endif %}
                {% elseif run.status == "failed" %}{_ Not completed _}
                {% elseif run.status == "interrupted" and run.prepared %}<strong>{_ Needs attention _}</strong>
                {% elseif run.status == "completed" or run.status == "completed_errors" %}{_ Done _}
                {% elseif run.status == "retrying" %}<strong>{_ Current phase — retrying temporary failures _}</strong>
                {% elseif run.prepared %}<strong>{_ Current phase _}</strong>
                {% else %}{_ Upcoming _}
                {% endif %}
            </td>
            <td>
                {% if run.first_submitted %}{{ run.first_submitted|date:"Y-m-d H:i:s" }} <span class="label label-default">{_ Sending started _}</span>
                {% elseif stopped %}{_ No emails submitted _}
                {% else %}{_ Starts after recipients are prepared. _}
                {% endif %}
            </td>
        </tr>
        <tr {% if run.finished %}class="{% if stopped or run.status == "completed_errors" %}warning{% else %}success{% endif %}"{% endif %}>
            <th scope="row">5. {_ Results _}</th>
            <td>
                {% if run.finished %}{% include "_mailing_run_status.tpl" status=run.status %}
                {% elseif run.status == "interrupted" %}{_ Waiting for the sending issue to be resolved _}
                {% else %}{_ Upcoming _}{% endif %}
            </td>
            <td>
                {% if run.finished %}
                    {{ run.finished|date:"Y-m-d H:i:s" }}
                    <span class="label label-default">
                        {% if run.status == "cancelled" %}{_ Stopped _}{% else %}{_ Finished _}{% endif %}
                    </span>
                    {% if run.stats.waiting and not run.details_expired %}
                        <br>{_ Results will still update for emails already queued. _}
                    {% endif %}
                {% else %}
                    {_ Available when sending and automatic retries finish. _}
                {% endif %}
            </td>
        </tr>
    </tbody>
</table>
</div>

{% if run.status == "interrupted" %}
    <p class="alert alert-warning">{_ Sending needs attention before the remaining phases can finish. See the actions below. _}</p>
{% endif %}
{% endwith %}
