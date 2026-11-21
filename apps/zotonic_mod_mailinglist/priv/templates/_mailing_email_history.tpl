{% if m.mailinglist_run.email_history[email]::%{offset:offset} as history %}
    <h3>{_ Recent mailing results _}: {{ history.email|escape }}</h3>
    <p><a href="{% url admin_mailinglist email=history.email %}">{_ View subscriptions and email status _}</a></p>
    <table class="table table-striped">
        <thead><tr>
            <th>{_ Last updated _}</th>
            <th>{_ Page _}</th>
            <th>{_ Mailing list _}</th>
            <th>{_ Result _}</th>
            <th>{_ Details _}</th>
        </tr></thead>
        <tbody>
        {% for result in history.results %}
            <tr>
                <td>{{ result.modified|date:"Y-m-d H:i" }}</td>
                <td>
                    <a href="{% url admin_mailing_run run_id=result.id recipient_email=history.email %}">{{ m.rsc[result.page_id].title }}</a>
                    {% if result.is_test %}<small>{_ Test email _}</small>{% endif %}
                </td>
                <td>{{ m.rsc[result.mailinglist_id].title }}</td>
                <td>{% include "_mailing_recipient_status.tpl" status=result.status %}</td>
                <td>{% include "_mailing_skip_reason.tpl" reason=result.reason|mailinglist_delivery_detail %}</td>
            </tr>
        {% empty %}
            <tr><td colspan="5" class="text-muted">{_ No recent mailing results found for this email address. _}</td></tr>
        {% endfor %}
        </tbody>
    </table>
    {% if history.next_offset %}
        <a class="btn btn-default" href="{% url admin_mailings email=history.email offset=history.next_offset %}">{_ Next page _}</a>
    {% endif %}
    {% if offset %}
        <a class="btn btn-default" href="{% url admin_mailings email=history.email %}">{_ First page _}</a>
    {% endif %}
    <p class="help-block">{_ Showing up to 200 recipient results per page from the last three months, including failed and skipped deliveries. Expired details and mailings from before run tracking are unavailable. Sent means accepted by the mail server; later bounces can still change the result. _}</p>
{% else %}
    <p class="alert alert-warning">{_ Enter a valid email address to look up mailing results. _}</p>
{% endif %}
