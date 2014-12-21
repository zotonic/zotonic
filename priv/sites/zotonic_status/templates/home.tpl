{% extends "base.tpl" %}

{% block title %}Zotonic {_ Sites _}{% endblock %}

{% block content %}
<div class="col-md-8">
    <div class="panel panel-default">
        <div class="panel-heading">
            <h3 class="panel-title">{_ Sites on this Zotonic server _}</h3>
        </div>
        <div class="panel-body">
            <div class="table-responsive">
                <table id="sites" class="table sites-overview">
                    {% include "_sites.tpl" %}
                </table>
            </div>
        </div>
    </div>
</div>
{% endblock %}


{% block sidebar %}
<div class="col-md-4">    
    <div class="panel panel-default">
        <div class="panel-heading">
            <h3 class="panel-title">{_ Commands _}</h3>
        </div>
        <div class="panel-body">        
            <div class="list-group">
                {% all include "_z_system_button.tpl" %}
                {% all include "_z_trace_button.tpl" %}
            </div>
        </div>
    </div>
    
    <div id="notices"></div>
</div>
{% endblock %}