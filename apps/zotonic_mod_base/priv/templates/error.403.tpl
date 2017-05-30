{% extends "base.tpl" %}

{% block title %}{_ No Access _}{% endblock %}

{% block content %}
<div class="row">
    <div class="col-md-1">
    </div>
    <div class="col-md-2">
        {% if m.req.referer or 1%}
          <h1>&nbsp;</h1>
          <p>
            <a rel="nofollow" href="{{ m.req.referer|escape }}" />{_ Go back _}</a>
          </p>
        {% endif %}
    </div>
    <div class="col-md-9">
        <h1>{_ No Access _}</h1>

        <p>
          {% if m.acl.user %}
              {_ To view this page, you need to have other access rights. _}<br/>
              {_ You may try to sign in as a different user. _}
          {% else %}
              {_ To view this page you must be signed in. _}
          {% endif %}
        </p>

        <p>
          <a class="btn btn-primary" href="{% url logon p=m.req.raw_path %}">{_ Sign in _}</a>
        </p>
      </div>
</div>
{% endblock %}
