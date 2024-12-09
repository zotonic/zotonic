{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing the time zone used for the resource #}

{% block widget_title %}
{_ Time zone _}
<div class="widget-header-tools">
    <a href="#" class="z-btn-help do_dialog" data-dialog="{{
            %{
                title: _"Time zone",
                text: _"This is the time zone in which the publication period and date range is entered. It defaults to your time zone. If you change the time zone then you will need to correct the publication period and date range manually."
            }|escape
        }}" title="{_ Need more help? _}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}{{ not id.tz or id.tz == m.req.timezone }}{% endblock %}
{% block widget_id %}sidebar-timezone{% endblock %}

{% block widget_content %}
    <p class="help-block">
        <i class="fa fa-info-circle"></i>
        {_ Changing the time zone will in effect change the publication period and date range. _}
        {_ All dates are stored in UTC. _}
    </p>
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
    <p class="help-block">{_ Your time zone is _}: {{ m.req.timezone|escape }}</p>
    {% javascript %}
        $('#rsc-tz').on('change', function() {
            let tz = $(this).val();
            $(".rsc-timezone")
                .text(tz)
                .closest('p').show();
        });
    {% endjavascript %}
{% endblock %}
