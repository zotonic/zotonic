{% extends "base.tpl" %}

{% block title %}{_ Select language _}{% endblock %}

{% block html_head_extra %}
    <meta name="robots" value="noindex">
{% endblock %}

{% block content %}

<h1>{_ Select your preferred language. _}</h1>

<ul class="language-switch nav nav-list">
    {% if m.rsc[q.id].id as id %}
        {% for code,lang in m.translation.language_list_enabled %}
            {% if code|member:id.language %}
                <li>
                    <a href="{{ id.page_url with z_language = code }}" class="translation">{{ lang.name }} <span class="fa fa-check"></span></a>
                </li>
            {% else %}
                <li>
                    <a href="{{ id.page_url with z_language = code }}" rel="nofollow">{{ lang.name }}</a>
                </li>
            {% endif %}
        {% endfor %}
    {% else %}
        {% for code,lang in m.translation.language_list_enabled %}
        	<li>
        	    <a href="{% url language_select code=code p=q.p %}" rel="nofollow">{{ lang.name }}</a>
        	</li>
        {% endfor %}
    {% endif %}
</ul>

{% endblock %}
