{% extends "base_noscript.tpl" %}

{% block _html_head %}
  {% lib
      "bootstrap/css/bootstrap.css"
      "css/jquery.loadmask.css"
      "css/z.growl.css"
      "css/z.modal.css"
  %}
{% endblock %}

{% block js_include %}
  {% include "_js_include.tpl" %}
  {% script %}
{% endblock%}
