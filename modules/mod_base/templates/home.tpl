{% extends "base.tpl" %}

{% block title %}{_ Welcome _}{% endblock %}

{% block content %}
<div class="hero-unit">
    <h1>Zotonic</h1>
    <p>Thank you for choosing Zotonic, and congratulations with
    your successful install. You see this page because you didn't
    enable a site module or the site module doesn't contain the
    <b>home.tpl</b> template.</p>

    <p>Go to the <a href="/admin/modules">Admin module pages</a> to enable the site module.</p>
    <p><a href="/admin" class="btn btn-primary">Visit the admin</a></p>
</div>
{% endblock %}
