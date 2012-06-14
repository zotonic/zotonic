{% extends "base.tpl" %}

{% block title %}{_ Search _}{% endblock %}

{% block content %}
{% if q.qs %}
{% with m.search.paged[{query text=q.qs cat=`text` cat=`collection` cat=`document` cat=`location` cat=`mailinglist` pagelen=10 page=q.page}] as result %}
    {% if result.total > 0 %}
        <p id="content-pager">
            <span class="pull-left">{_ Searching for _} <b>{{ q.qs|escape }}</b></span>
            &nbsp;
            <span class="pull-right">
                {_ Page _} {{ result.page }}/{{ result.pages }}
            </span>
        </p>
        {% include "_content_list.tpl" list=result %}
        {% pager result=result %}
    {% else %}
    <p id="content-pager">
        {_ Did not find any pages matching _} <b>{{ q.qs|escape }}</b>.
    </p>
    {% endif %}
{% endwith %}

<hr/>
{% endif %}

<form class="well" method="GET" action="{% url search %}#content-pager">
    <fieldset>
        <label>{_ Type the text to search _}</label>
        <input type="text" name="qs" value="{{ q.qs|escape }}" />
    </fieldset>
    <input class="btn btn-primary" type="submit" value="{_ Search _}" />
</form>

{% endblock %}
