{% extends "base.tpl" %}

{% block title %}{_ Image clipper _}{% endblock %}

{% block content %}
<h1>{_ Image clipper _}</h1>
<p>
    {_ To use the clipper, drag the following link to your bookmarks bar: _}
    <a href='{% include "_clipper_bookmarklet.tpl" %}'>{_ Clip to _} {{ m.site.title }}</a>
</p>
{% endblock %}
