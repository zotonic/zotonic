{% extends "admin_edit_widget_i18n.tpl" %}

{# Widget for editing abstract event date_start/date_end #}

{% block widget_title %}
{_ Date range _}
<div class="widget-header-tools">
    <a href="javascript:void(0)" class="z-btn-help do_dialog" data-dialog="title: '{{ _"Date ranges"|escapejs }}', text: '{{ _"Every page can have a date range. For example if the page is an event or description of someone’s life."|escapejs }}'" title="{_ Need more help? _}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}{{ not ((id.date_start|in_past and id.date_end|in_future) or id.is_a.event or id.is_a.survey) }}{% endblock %}
{% block widget_id %}sidebar-date-range{% endblock %}

{% block widget_content %}
<fieldset>
    <div class="form-group row">
        <div class="col-md-12">
            <label class="control-label">{_ Remarks _} {{ lang_code_with_brackets }}</label>
            <div>
                <input type="text" id="{{ #remarks }}{{ lang_code_for_id }}" name="date_remarks{{ lang_code_with_dollar }}"
                    value="{{ is_i18n|if : id.translation[lang_code].date_remarks : id.date_remarks }}"
                    {% if not id.is_editable %}disabled="disabled"{% endif %}
                    {% include "_language_attrs.tpl" language=lang_code class="field-title form-control" %}
                    placeholder="{_ e.g. might change _}"
                />
            </div>
        </div>
    </div>
</fieldset>
{% endblock %}

{% block widget_content_nolang_before %}
<div class="date-range">
    <fieldset>
        <div class="row">
            <div class="col-md-12">
                <div class="checkbox">
                    <label>
                        <input name="date_is_all_day" id="{{ #all_day }}" type="checkbox" {% if id.date_is_all_day %}checked{% endif %} /> {_ All day _}
                    </label>
                </div>
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
        <hr />
        <div class="row">
            <div class="col-md-6">
                <div>
                    <label class="control-label">{_ From _}</label>
                    <div>
                        {% include "_edit_date.tpl" date=id.date_start name="date_start" is_end=0 date_is_all_day=id.date_is_all_day  is_editable=id.is_editable %}
                    </div>
                </div>
            </div>
            <div class="col-md-6">
                <div>
                    <label class="control-label">{_ Till _}</label>
                    <div>
                        {% include "_edit_date.tpl" date=id.date_end name="date_end" is_end=1 date_is_all_day=id.date_is_all_day  is_editable=id.is_editable %}
                    </div>
                </div>
            </div>
        </div>
    </fieldset>
</div>
{% endblock %}
