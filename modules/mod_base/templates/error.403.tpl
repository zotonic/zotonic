{% extends "base.tpl" %}

{% block title %}{_ No Access _}{% endblock %}

{% block content %}
  <h1>{_ No Access _}</h1>

  {% if m.acl.user %}
    <p>{_ Sorry, you don’t have access to this page. _}</p>
  {% else %}
    <p>{_ This page is only visible for authenticated users, log on to view this page. _}</p>
  {% endif %}

  <p>
    {% if m.req.referer %}
      <a class="btn btn-primary" rel="nofollow" href="{{ m.req.referer|escape }}" />{_ Back _}</a>
    {% endif %}
    <a class="btn btn-default" rel="nofollow" href="{% url logon p=m.req.raw_path %}" />
      {% if m.acl.user %}{_ Log on as a different user _}{% else %}{_ Log On _}{% endif %}
    </a>
  <p>
{% endblock %}
