{% extends "admin_base.tpl" %}

{% block title %}{_ Configuration _}{% endblock %}

{% block content %}

<div class="admin-header">
    <h2>{_ System Configuration _}</h2>
</div>

<div class="well">
    {% button class="btn btn-primary" text=_"Make a new config setting" action={dialog_config_new on_success={reload}} %}
</div>

<div class="widget">
    <table class="table table-striped do_adminLinkedTable">
        <thead>
            <tr>
                <th width="15%">{_ Module _}</th>
                <th width="20%">{_ Key _}</th>
                <th width="35%">{_ Value _}</th>
                <th width="30%">{_ Modified _}</th>
            </tr>
        </thead>

        <tbody>
            {% for g, keys in config %}
            {% for k, c in keys %}
            {% with c.id,
                    g|default:'',
                    k|default:'',
                    c.value,
                    c.is_secret
               as
                    id,
                    module,
                    key,
                    value,
                    is_secret
            %}
            {% with
               {dialog_config_edit module=module key=key value=value on_success={reload}},
               {dialog_config_delete module=module key=key on_success={slide_fade_out target=#tr.id}}
               as
               updateAction,
               deleteAction
            %}
            <tr id="{{ #tr.id }}" class="clickable" data-entry="{{module}}.{{key}}">
                <td>{{ module|escape|default:"-" }}</td>
                <td>{{ key|escape|default:"-" }}</td>
                <td>
                    {% if value == '' %}
                        <span class="text-muted">&mdash;</span>
                    {% elseif is_secret %}
                        {{ value|escape|truncatechars:3:" <span class='text-muted'>...</span> " }}
                        <span class="text-muted">
                            <i class="glyphicon glyphicon-eye-close"></i>
                        </span>
                    {% else %}
                        {{ value|escape|truncatechars:65 }}
                    {% endif %}
                </td>
                <td>
                    <div class="pull-right buttons">
                        {% button class="btn btn-default btn-xs" text=_"Delete" action=deleteAction %}
                        {% button class="btn btn-default btn-xs" text=_"Edit" action=updateAction %}
                    </div>
                    {{ c.modified|date:_"d M Y, H:i" }}
                </td>
            </tr>
            {% wire id=#tr.id action=updateAction %}

            {% endwith %}
            {% endwith %}
            {% endfor %}
            {% empty %}
            <tr>
                <td colspan="4">
                    {_ No configurations found. _}
                </td>
            </tr>
            {% endfor %}
        </tbody>
    </table>
</div>

{% endblock %}
