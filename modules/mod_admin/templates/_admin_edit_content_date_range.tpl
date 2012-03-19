{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing abstract event date_start/date_end #}

{% block widget_title %}{_ Date range _}{% endblock %}
{% block widget_show_minimized %}{% with m.rsc[id] as r %}{{ not ((r.date_start|in_past and r.date_end|in_future) or r.is_a.event or r.is_a.survey) }}{% endwith %}{% endblock %}
{% block widget_id %}sidebar-date-range{% endblock %}

{% block widget_content %}

<div class="pull-right">
    <a href="javascript:void(0)" class="btn btn-primary btn-mini do_dialog" data-dialog="title: '{{ _"Help about date ranges."|escapejs }}', text: '{{ _"Every page can have a date range. For example if the page is an event or description of someone\'s life."|escapejs }}'" title="{_ Need more help? _}"><i class="icon-question-sign icon-white"></i></a>
</div>


{% with m.rsc[id] as r %}
<div class="control-group">
    <label class="control-label">{_ From _}</label>
    <div class="controls">
	{% include "_edit_date.tpl" date=r.date_start name="date_start" is_end=0 %}
    </div>
</div>

<div class="control-group">
    <label class="control-label">{_ Till _}</label>
    <div class="controls">
        {% include "_edit_date.tpl" date=r.date_end name="date_end" is_end=1 %}
    </div>
</div>

<div class="control-group">
    <label class="control-label">{_ Remarks _}</label>
    <div class="controls">
	<input type="text" name="date_remarks" value="{{ r.date_remarks }}" />
    </div>
</div>

{% endwith %}
{% endblock %}
