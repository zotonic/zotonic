{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing the time zone used for the resource #}

{% block widget_title %}
{_ Time zone _}
<div class="widget-header-tools">
    <a href="javascript:void(0)" class="z-btn-help do_dialog" data-dialog="title: '{{ _"Time zone"|escapejs }}', text: '{{ _"This is the time zone in which the publication period and date range is entered. It defaults to your time zone. If you change the time zone then you will need to correct the publication period and date range manually."|escapejs }}'" title="{_ Need more help? _}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}{{ not id.tz or id.tz == m.req.timezone }}{% endblock %}
{% block widget_id %}sidebar-timezone{% endblock %}

{% block widget_content %}
    <fieldset>
        <div class="form-group">
            <label class="control-label">{_ Time zone for publication period and date range _}</label>
            <div>
                <select id="rsc-tz" name="tz" class="form-control">
                {% with id.tz|default:m.req.timezone as timezone %}
                    {% for tz in m.l10n.timezones %}
                        <option {% if timezone == tz %}selected{% endif %}>{{ tz }}</option>
                    {% endfor %}
                {% endwith %}
                </select>
            </div>
        </div>
    </fieldset>

    <p class="help-block">
        {_ Changing the time zone does not affect the entered dates. _}
    </p>

{% endblock %}
