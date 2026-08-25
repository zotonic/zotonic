{% extends "admin_base.tpl" %}

{% block title %}{_ Mailing Lists _}{% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ Mailing lists _}</h2>

    <p>{_ Any page can be sent as a mailing. You can send a mailing from any edit page. On this page you can add or remove mailing lists and their recipients. _}<br/>
    {_ Recipients are subscribed either as email-only (via a simple signup form), or as subscribed persons in the system. _}</p>
</div>

<div class="well z-button-row">
    {% button class="btn btn-primary" text=_"New mailing list" action={dialog_new_rsc cat="mailinglist"} %}
</div>


{% live topic=["bridge", "origin", "model", "mailinglist", "event", "+", "scheduled"]
        template="_admin_mailinglist_overview.tpl"
%}

{% endblock %}
