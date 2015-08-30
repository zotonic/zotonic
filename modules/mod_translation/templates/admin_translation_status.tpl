{% extends "admin_base.tpl" %}

{% block title %} {_ Translation status _} {% endblock %}

{% block content %}
<style type="text/css">
.perc-0 { background-color: #fc5830 !important; color: white; }
.perc-20 { background-color: #fd9727 !important; color: white; }
.perc-40 { background-color: #fdc02f !important; color: white; }
.perc-60 { background-color: #fee94e !important; color: white; }
.perc-80 { background-color: #afe82e !important; color: white; }
.perc-100 { background-color: #69db30 !important; color: white;}
</style>

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
    
    <table class="table">
        <thead>
            <tr>
                <th width="1%"></th>
                <th width="30%">{_ Title _}</th>
                {% for code, lang in m.config.i18n.language_list.list %}
                {% if code != "en" %}
                <th>{{ lang.language }}</th>
                {% endif %}
                {% endfor %}
            </tr>
        </thead>

        <tbody>

	    {% for sort, prio, module, props in modules %}
	    <tr id="{{ #li.module }}">
		    <td>{% include "_icon_status.tpl" status=status[module] status_id=#status.module %}</td>
		    <td>{{ props.mod_title|default:props.title }}</td>
                {% for code, lang in m.config.i18n.language_list.list %}
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
