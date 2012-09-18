<!DOCTYPE html>
<html lang="{{ z_language|default:"en"|escape }}">
  <head>
    <meta charset="utf-8" />
    <title>Zotonic{% block title %}{% endblock %}</title>
    
    <link rel="icon" href="/favicon.ico" type="image/x-icon" />
    <link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" />
    
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <meta name="author" content="Arjan Scherpenisse, Marc Worrell" />
  </head>
  
  <body>
    {% block content_area %}
    {% block content %}{% endblock %}
    {% endblock %}
  </body>
</html>
