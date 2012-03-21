{% extends "admin_log_base.tpl" %}

{% block title %}{_ Log messages _}{% endblock %}

{% block title_log %}{_ Log messages _}{% endblock %}

{% block active1 %}active{% endblock %}

{% block content_log %}

<h3 class="above-list ">
    {_ Most recent messages _}
</h3>
<br />

{% with m.search[{log page=q.page pagelen=20}] as result %}
<div id="log-area">
    {% for id in result %}
    {% include "_admin_log_row.tpl" id=id %}
    {% empty %}
    <div class="alert alert-info">
	{_ No log messages. _}
    </div>
    {% endfor %}
</div>
{% button class="btn btn-primary" text="more..." action={moreresults result=result target="log-area" template="_admin_log_row.tpl"} %}
{% endwith %}

{% wire action={connect signal={log_message} action={addlog target="log-area"}} %}

{% endblock %}
