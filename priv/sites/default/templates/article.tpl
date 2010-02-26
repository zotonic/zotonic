{% extends "page.tpl" %}

{% block title %}{{ m.rsc[id].title }}{% endblock %}

{% block content %}

<div class="post zp-100">
    <h1>{{ m.rsc[id].title }}</h1>
    {% include "_article_meta.tpl" id=id %}
    
    <p class="summary">
        {{ m.rsc[id].summary }}
    </p>

    {{ m.rsc[id].body|show_media }}
</div>

{% include "_article_keywords.tpl" id=id %}
<section id="comments">{% include "_comments.tpl" id=id %}</section>
{% include "_article_prevnext.tpl" id=id %}

{% endblock %}
