{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing abstract event date_start/date_end #}

{% block widget_title %}{_ Date range _}{% endblock %}
{% block widget_show_minimized %}{% with m.rsc[id] as r %}{{ not ((r.date_start|in_past and r.date_end|in_future) or r.is_a.event or r.is_a.survey) }}{% endwith %}{% endblock %}


{% block widget_content %}
{% with m.rsc[id] as r %}
<div class="admin-form form-item">
	<div class="notification notice">
		{_ Used for events and other periods. _}
		<a href="javascript:void(0)" class="do_dialog" data-dialog="title: '{{ _"Help about date ranges."|escapejs }}', text: '{{ _"Every page can have a date range. For example if the page is an event or description of someone\'s life."|escapejs }}', width: '450px'">{_ Need more help? _}</a>
	</div>
	<fieldset>
		<div class="form-item">
			<label>{_ From _}</label>
			{% include "_edit_date.tpl" date=r.date_start name="date_start" is_end=0 %}
		</div>
		<div class="form-item">
			<label>{_ Till _}</label>
			{% include "_edit_date.tpl" date=r.date_end name="date_end" is_end=1 %}
		</div>
		<div class="form-item clear">
			<label>{_ Remarks _}</label>
			<input type="text" name="date_remarks" value="{{ r.date_remarks }}" />
		</div>
	</fieldset>
</div>
{% endwith %}
{% endblock %}
