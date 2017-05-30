{% extends "admin_base.tpl" %}

{% block title %} {_ Translation status _} {% endblock %}

{% block content %}
<ul class="breadcrumb">
    <li><a href="{% url admin_translation %}">{_ Languages overview _}</a></li>
    <li class="active">{_ Translation status _}</li>
</ul>

<div class="admin-header">
    <h2>{_ Translation status per module _}</h2>
    <p>{_ To view the up-to-date version of this module, please run "generate .pot files" first from the translation overview. _}</p>
</div>

<div>
    <h3>{_ Active modules _}</h3>

    <table class="table table-condensed mod_translation-status-table">
        <thead>
            <tr>
                <th width="1%"></th>
                <th>{_ Title _}</th>
                {% for code, lang in m.translation.language_list_configured %}
                    {% if code != 'en' %}
                        <th>{{ code }}</th>
                    {% endif %}
                {% endfor %}
            </tr>
        </thead>

        <tbody>

	    {% for sort, prio, module, props in modules %}
	    <tr id="{{ #li.module }}">
		    <td>{% include "_icon_status.tpl" status=status[module] status_id=#status.module %}</td>
		    <td class="mod_translation-status-title">{{ props.module|default:props.title }}</td>
            {% for code, lang in m.translation.language_list_configured %}
                {% if code != "en" %}
                    {% admin_translation_statistics module=module lang=code %}
                {% endif %}
            {% endfor %}
            </tr>
            {% endfor %}
        </tbody>
    </table>
</div>

{% endblock %}
