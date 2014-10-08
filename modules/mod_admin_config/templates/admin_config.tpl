{% extends "admin_base.tpl" %}

{% block title %}{_ Configuration _}{% endblock %}

{% block content %}

<div class="edit-header">
    <h2>{_ System Configuration _}</h2>

    <div class="well">
        {% button class="btn btn-primary" text=_"Make a new config setting" action={dialog_config_new on_success={reload}} %}
    </div>

</div>
<div>
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
            {% for m, keys in config %}
            {% for k, c in keys %}
            {% with c.id,
                    m|default:'',
                    k|default:'',
                    c.value
               as
                    id,
                    module,
                    key,
                    value
            %}
            {% with
               {dialog_config_edit module=module key=key value=value on_success={reload}},
               {dialog_config_delete module=module key=key on_success={slide_fade_out target=#tr.id}}
               as
               updateAction,
               deleteAction
            %}
            <tr id="{{ #tr.id }}" class="clickable" data-href="#" data-entry="{{module}}.{{key}}">
                <td>{{ module|escape|default:"-" }}</td>
                <td>{{ key|escape|default:"-" }}</td>
                <td>{{ value|escape|default:"-"|truncate:65 }}</td>
                <td>
                    <div class="pull-right">
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
