{% extends "logon.tpl" %}

{% block title %}{_ No Access _}{% endblock %}

{% block content_area %}
    {% with (not m.config.mod_ssl.is_ssl.value or m.req.is_ssl) or m.config.site.protocol.value|default:"http" /= 'http' as is_show_login %}
      {% if m.acl.user %}
          <h1>{_ No Access _}</h1>
          <p>
              {_ Sorry, you don’t have access to this page. _}

              {% if is_show_login and m.req.referer %}
                <a class="btn btn-primary btn-sm" rel="nofollow" href="{{ m.req.referer|escape }}" />{_ Back _}</a>
              {% endif %}
          </p>
      {% else %}
          <h1>{_ You need to log on _}</h1>
          <p>
              {_ This page is only visible for authenticated users, log on to view this page. _}

              {% if is_show_login and m.req.referer %}
                <a class="btn btn-primary btn-sm" rel="nofollow" href="{{ m.req.referer|escape }}" />{_ Back _}</a>
              {% endif %}
          </p>
      {% endif %}

      {% if is_show_login %}
          {% with '#reload', 1 as page, use_wire %}
              {% inherit %}
          {% endwith %}
      {% else %}
          {# We are on insecure page and the site has ssl enabled #}
          <p>
              {% if m.req.referer %}
                <a class="btn btn-primary" rel="nofollow" href="{{ m.req.referer|escape }}" />{_ Back _}</a>
              {% endif %}
              <a class="btn btn-default" rel="nofollow" href="{% url logon p=m.req.raw_path %}" />
                {% if m.acl.user %}{_ Log on as a different user _}{% else %}{_ Log On _}{% endif %}
              </a>
          </p>
      {% endif %}
  {% endwith %}

{% endblock %}
