{% extends "logon.tpl" %}

{% block title %}{_ No Access _}{% endblock %}

{% block page_class %}err403{% endblock %}

{% block content_area %}
    {% if m.req.referer %}
      <div class="z-logon-back">
          <a rel="nofollow" href="#back" data-onclick-topic="model/location/post/redirect/back"/>{_ Go back _}</a>
      </div>
    {% endif %}

    <div class="z-logon-prompt text-muted">
        {% if m.acl.user %}
            {_ To view this page, you need to have other access rights. _}<br/>
            {_ You may try to login as a different user. _}
        {% else %}
            {_ To view this page you must be logged in. _}
        {% endif %}
    </div>

    {% with '#reload' as page %}
        {% inherit %}
    {% endwith %}
{% endblock %}
