{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing rsc publication date_start/date_end #}

{% block widget_title %}{_ Publication period _}{% endblock %}
{% block widget_show_minimized %}{{ r.publication_start|in_past and r.publication_end|in_future }}{% endblock %}
{% block widget_id %}sidebar-pub-period{% endblock %}

{% block widget_content %}
<fieldset class="form-horizontal">
    <a href="javascript:void(0)" class="z-btn-help do_dialog" data-dialog="title: '{{ _"Help about publication date range"|escapejs }}', text: '{{ _"When a page has a publication date range then it will only be visible between the two dates. Note that when you are allowed to edit the page then you can always see it."|escapejs }}'" title="{_ Need more help? _}"></a>

    {% with m.rsc[id] as r %}
        <div class="form-group row">
            <label class="control-label col-md-5">{_ Visible from _}</label>
            <div class="col-md-7">
                {% include "_edit_date.tpl" date=r.publication_start name="publication_start" is_end=0 %}
            </div>
        </div>
        <div class="form-group row">
            <label class="control-label col-md-5">{_ Visible till _}</label>
            <div class="col-md-7">
	            {% include "_edit_date.tpl" date=r.publication_end name="publication_end" is_end=1 %}
            </div>
        </div>
        <div class="form-group row">
            <label class="control-label col-md-5">{_ Publication date of original article _}</label>
            <div class="col-md-7">
	            {% include "_edit_date.tpl" date=r.org_pubdate name="org_pubdate" is_end=0 %}
            </div>
        </div>
    {% endwith %}
</fieldset>
{% endblock %}
