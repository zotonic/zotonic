{% extends "page.tpl" %}

{% block title %}{{ m.rsc[id].title }}{% endblock %}

{% block content %}
<h1>{{ m.rsc[id].title }}</h1>
{% include "_article_meta.tpl" id=id %}

<p class="summary">
    {{ m.rsc[id].summary }}
</p>

{{ m.rsc[id].body|show_media }}

{% include "_article_keywords.tpl" id=id %}

{% endblock %}
