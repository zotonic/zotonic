{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing rsc publication date_start/date_end #}

{% block widget_title %}{_ Publication period _}{% endblock %}
{% block widget_show_minimized %}{{ r.publication_start|in_past and r.publication_end|in_future }}{% endblock %}


{% block widget_content %}

<div class="pull-right">
    <a href="javascript:void(0)" class="btn btn-primary btn-mini do_dialog" data-dialog="title: '{{ _"Help about publication date range."|escapejs }}', text: '{{ _"When a page has a publication date range then it will only be visible between the two dates. Note that when you are allowed to edit the page then you can always see it."|escapejs }}'" title="{_ Need more help? _}"><i class="icon-question-sign icon-white"></i></a>
</div>

{% with m.rsc[id] as r %}
<div class="control-group">
    <label class="control-label">{_ Visible from _}</label>
    <div class="controls">
        {% include "_edit_date.tpl" date=r.publication_start name="publication_start" is_end=0 %}
    </div>
</div>
<div class="control-group">
    <label class="control-label">{_ Visible till _}</label>
    <div class="controls">
	{% include "_edit_date.tpl" date=r.publication_end name="publication_end" is_end=1 %}
    </div>
</div>
{% endwith %}
{% endblock %}
