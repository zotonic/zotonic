{% extends "base.tpl" %}

{% block title %}{_ Sites _}{% endblock %}

{% block content %}
<div class="row">
<div class="span8">
    <h1>{_ Sites on this Zotonic server _}</h1>
	
    <table id="sites" class="table">
	{% include "_sites.tpl" %}
    </table>
</div>


<div class="span4">
    <div class="well">
	{% all include "_z_system_button.tpl" %}
        {% all include "_z_trace_button.tpl" %}
    </div>
    <div style="clear:left" id="notices"></div>
</div>
</div>
{% endblock %}



