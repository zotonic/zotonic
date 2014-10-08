{% extends "admin_edit_widget_i18n.tpl" %}

{# Widget for editing abstract event date_start/date_end #}

{% block widget_title %}{_ Date range _}{% endblock %}
{% block widget_show_minimized %}{% with m.rsc[id] as r %}{{ not ((r.date_start|in_past and r.date_end|in_future) or r.is_a.event or r.is_a.survey) }}{% endwith %}{% endblock %}
{% block widget_id %}sidebar-date-range{% endblock %}

{% block widget_content %}
    <div class="form-group">
        <label class="control-label" for="{{ #remarks }}{{ lang_code_for_id }}">{_ Remarks _} {{ lang_code_with_brackets }}</label>
        <div>
            <input type="text" id="{{ #remarks }}{{ lang_code_for_id }}" name="date_remarks{{ lang_code_with_dollar }}" 
                value="{{ is_i18n|if : id.translation[lang_code].date_remarks : id.date_remarks }}"
                {% if not is_editable %}disabled="disabled"{% endif %}
                {% include "_language_attrs.tpl" language=lang_code class="do_autofocus field-title form-control" %}
                placeholder="{_ e.g. might change _}"
            />
        </div>
    </div>
{% endblock %}

{% block widget_content_nolang_before %}
    <div class="pull-right">
        <a href="javascript:void(0)" class="btn btn-primary btn-xs do_dialog" data-dialog="title: '{{ _"Help about date ranges."|escapejs }}', text: '{{ _"Every page can have a date range. For example if the page is an event or description of someone’s life."|escapejs }}'" title="{_ Need more help? _}"><i class="glyphicon glyphicon-question-sign"></i></a>
    </div>

    <div class="date-range">
        <div class="form-group">
            <div class="checkbox">
                <label>
                    <input name="date_is_all_day" id="{{ #all_day }}" type="checkbox" {% if id.date_is_all_day %}checked{% endif %} /> {_ All day _}
                </label>
            </div>
        </div>
        {% javascript %}
            $("#{{ #all_day }}").on('change', function() {
                var $times = $(this).closest('.date-range').find('.do_timepicker');
                if ($(this).is(":checked"))
                    $times.fadeOut("fast").val('');
                else
                    $times.fadeIn("fast");
            });
        {% endjavascript %}

        <div class="form-group">
            <label class="control-label">{_ From _}</label>
            <div>
        	{% include "_edit_date.tpl" date=id.date_start name="date_start" is_end=0 date_is_all_day=id.date_is_all_day %}
            </div>
        </div>
        <div class="form-group">
            <label class="control-label">{_ Till _}</label>
            <div>
                {% include "_edit_date.tpl" date=id.date_end name="date_end" is_end=1 date_is_all_day=id.date_is_all_day %}
            </div>
        </div>
    </div>
{% endblock %}
