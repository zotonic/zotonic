{% extends "base_noscript.tpl" %}

{% block _html_head %}
  {% include "_html_head_cotonic.tpl" %}
  {% lib
      "bootstrap/css/bootstrap.css"
      "css/jquery.loadmask.css"
      "css/z.growl.css"
      "css/z.modal.css"
  %}
  {% all include "_html_head.tpl" %}
{% endblock %}

{% block _js_include %}
  {% include "_js_include.tpl" %}
  {% script %}
{% endblock%}
