<!DOCTYPE html>
<html lang="{{ z_language|default:"en"|escape }}">
  <head>
    <meta charset="utf-8" />
    <title>{% block title %}Zotonic{% endblock %}</title>
    
    <link rel="icon" href="/favicon.ico" type="image/x-icon" />
    <link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" />
    
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <meta name="author" content="Arjan Scherpenisse, Marc Worrell" />

    {% block _html_head %}
      {% lib 
              "bootstrap/css/bootstrap.css" 
              "css/jquery.loadmask.css" 
              "css/z.growl.css" 
              "css/z.modal.css" 
      %}
    {% endblock %}
  </head>
  
  <body>
    {% block content_area %}
      {% block content %}{% endblock %}
      {% include "_js_include.tpl" %}
    {% endblock %}
    {% script %}
  </body>
</html>
