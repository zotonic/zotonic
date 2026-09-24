{% extends "admin_base.tpl" %}
{% block title %}{_ Mailing status _}{% endblock %}
{% block content %}
<p><a href="{% url admin_mailings %}">{_ Mailings _}</a> / {_ Mailing status _}</p>
{% if m.mailinglist_run.run[q.run_id] as run %}
    {% live topic=["bridge","origin","model","mailinglist","event",run.page_id,"runs"]
        template="_admin_mailing_run.tpl" run_id=run.id recipient_status=q.recipient_status after=q.after %}
<h3>{_ Recipient results _}</h3>
<form method="get">
    <label>{_ Recipient status _} <select name="recipient_status" class="form-control">
        <option value="">{_ All statuses _}</option>
        {% for state in ["pending","submitting","queued","retrying","sent","failed","bounced","skipped","cancelled"] %}
        <option value="{{ state }}" {% if q.recipient_status == state %}selected{% endif %}>{% include "_mailing_recipient_status.tpl" status=state %}</option>
        {% endfor %}
    </select></label>
    <button type="submit" class="btn btn-default">{_ Filter _}</button>
</form>
{% live topic=["bridge","origin","model","mailinglist","event",run.page_id,"runs"]
        template="_mailing_run_recipients.tpl" run_id=run.id recipient_status=q.recipient_status after=q.after %}
{% else %}<p>{_ This mailing is unavailable or you do not have permission to view it. _}</p>{% endif %}
{% endblock %}
