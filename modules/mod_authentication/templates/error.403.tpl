{% extends "logon.tpl" %}

{% block title %}{_ No Access _}{% endblock %}

{% block page_class %}err403{% endblock %}

{% block content_area %}
    {% with (not m.config.mod_ssl.is_ssl.value or m.req.is_ssl)
            or m.config.site.protocol.value|default:"http" /= 'http'
       as is_show_login
    %}
      {% if m.req.referer %}
        <div class="z-logon-back">
            <a rel="nofollow" href="{{ m.req.referer|escape }}" />{_ Go back _}</a>
        </div>
      {% endif %}

      <div class="z-logon-prompt text-muted">
          {% if m.acl.user %}
              {_ To view this page, you need to have other access rights. _}<br/>
              {_ You may try to sign in as a different user. _}
          {% else %}
              {_ To view this page you must be signed in. _}
          {% endif %}
          {% if not is_show_login %}
              {# We are on insecure page and the site has ssl enabled #}
              <br/>
              <br/>
              <a href="{% url logon p=m.req.raw_path %}">{_ Sign in _}</a>
          {% endif %}
      </div>

      {% if is_show_login %}
          {% with '#reload', 1 as page, use_wire %}
              {% inherit %}
          {% endwith %}
      {% endif %}
  {% endwith %}
{% endblock %}
