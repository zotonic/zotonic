{% extends "base_noscript.tpl" %}

{% block _html_head %}
  {% lib
      "bootstrap/css/bootstrap.css"
      "css/jquery.loadmask.css"
      "css/z.growl.css"
      "css/z.modal.css"
  %}
  {% block html_head_extra %}
  {% endblock %}
{% endblock %}

{% block _js_include %}
  {% include "_js_include.tpl" %}
  {% script %}
{% endblock%}
