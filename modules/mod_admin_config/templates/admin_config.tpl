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
    <h3>{_ Configuration overview _}</h3>
    <hr />

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
            {% for module, keys in config %}
            {% for key, c in keys %}
            {% with c.id as id %}
            <tr id="{{ #tr.id }}" class="clickable"  data-href="#">
                    <td>{{ module|escape|default:"-" }}</td>
                    <td>{{ key|escape|default:"-" }}</td>
                    <td>{{ c.value|escape|default:"-" }}</td>
                    <td>
                        <div class="pull-right">
                            {% button class="btn btn-mini" text=_"Delete" action={dialog_config_delete module=module key=key on_success={slide_fade_out target=#tr.id}} %}
                            {% button class="btn btn-mini" text=_"Edit" action={dialog_config_edit module=module key=key on_success={reload}} %}
                        </div>
                        
                        {{ c.modified|date:"d M Y, H:i" }}
                    </td>
            </tr>
            {% wire id=#tr.id action={dialog_config_edit module=module key=key on_success={reload}} %}

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
