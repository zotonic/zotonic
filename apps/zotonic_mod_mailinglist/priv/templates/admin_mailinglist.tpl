{% extends "admin_base.tpl" %}

{% block title %}{_ Mailing Lists _}{% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ Mailing lists _}</h2>

    <p>{_ Any page can be sent as a mailing. You can send a mailing from any edit page. On this page you can add or remove mailing lists and their recipients. _}<br/>
    {_ Recipients are subscribed either as email-only (via a simple signup form), or as subscribed persons in the system. _}</p>
</div>

{% include "_mailinglist_test_public_alert.tpl" %}

<div class="well z-button-row">
    <a class="btn btn-default" href="{% url admin_mailings %}">{_ Mailing status and history _}</a>
    {% button class="btn btn-primary" text=_"New mailing list" action={dialog_new_rsc cat="mailinglist"} %}
</div>


<form method="get" action="{% url admin_mailinglist %}" class="form-inline well">
    <label>{_ Find subscriptions by email address _}
        <input type="email" name="email" class="form-control" value="{{ q.email|escape }}" required>
    </label>
    <button type="submit" class="btn btn-default">{_ Find _}</button>
    {% if q.email %}<a href="{% url admin_mailinglist %}" class="btn btn-default">{_ Clear _}</a>{% endif %}
</form>
{% if q.email %}
    {% include "_mailinglist_email_lookup.tpl" email=q.email %}
{% endif %}

{% live topic=["bridge", "origin", "model", "mailinglist", "event", "+", "+"]
        throttle=3000
        template="_admin_mailinglist_overview.tpl"
%}

{% endblock %}
