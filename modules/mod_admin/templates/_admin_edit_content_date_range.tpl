{% extends "admin_edit_widget_i18n.tpl" %}

{# Widget for editing abstract event date_start/date_end #}

{% block widget_title %}{_ Date range _}{% endblock %}
{% block widget_show_minimized %}{% with m.rsc[id] as r %}{{ not ((r.date_start|in_past and r.date_end|in_future) or r.is_a.event or r.is_a.survey) }}{% endwith %}{% endblock %}
{% block widget_id %}sidebar-date-range{% endblock %}

{% block widget_content %}
    <div class="control-group">
        <label class="control-label" for="{{ #remarks }}{{ lang_code_for_id }}">{_ Remarks _} {{ lang_code_with_brackets }}</label>
        <div class="controls">
            <input type="text" id="{{ #remarks }}{{ lang_code_for_id }}" name="date_remarks{{ lang_code_with_dollar }}" 
                value="{{ is_i18n|if : id.translation[lang_code].date_remarks : id.date_remarks }}"
                {% if not is_editable %}disabled="disabled"{% endif %}
                {% include "_language_attrs.tpl" language=lang_code class="do_autofocus input-block-level field-title" %}
                placeholder="{_ e.g. might change _}"
            />
        </div>
    </div>
{% endblock %}

{% block widget_content_nolang_before %}
    <div class="pull-right">
        <a href="javascript:void(0)" class="btn btn-primary btn-mini do_dialog" data-dialog="title: '{{ _"Help about date ranges."|escapejs }}', text: '{{ _"Every page can have a date range. For example if the page is an event or description of someone’s life."|escapejs }}'" title="{_ Need more help? _}"><i class="icon-question-sign icon-white"></i></a>
    </div>

    {% with (not id.date_start|date:"H:i" and not id.date_end|date:"H:i") as is_whole_day %}
    <div class="date-range">
        <div class="control-group">
            <div class="controls">
                <label class="checkbox">
                    <input id="{{ #whole_day }}" type="checkbox" {% if is_whole_day %}checked{% endif %} /> {_ Whole day _}
                </label>
            </div>
        </div>
        {% javascript %}
            $("#{{ #whole_day }}").on('change', function() {
                var $times = $(this).closest('.date-range').find('.do_timepicker');
                if ($(this).is(":checked"))
                    $times.fadeOut("fast").val('');
                else
                    $times.fadeIn("fast");
            });
        {% endjavascript %}

        <div class="control-group">
            <label class="control-label">{_ From _}</label>
            <div class="controls">
        	{% include "_edit_date.tpl" date=id.date_start name="date_start" is_end=0 is_whole_day=is_whole_day %}
            </div>
        </div>
        <div class="control-group">
            <label class="control-label">{_ Till _}</label>
            <div class="controls">
                {% include "_edit_date.tpl" date=id.date_end name="date_end" is_end=1 is_whole_day=is_whole_day %}
            </div>
        </div>
    </div>
    {% endwith %}
{% endblock %}
