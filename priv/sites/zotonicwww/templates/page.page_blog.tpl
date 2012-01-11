{% extends "page.tpl" %}

{% block below_content %}

{% with m.search[{query cat='article' sort='-publication_start'}] as articles %}
{% for id in articles %}
{% include "_article_summary.tpl" %}
{% endfor %}

{% endwith %}

{% endblock %}

{% block sidebar %}
<aside id="sidebar" class="zp-33">
    {% include "_sidebar.article.tpl" %}
</aside>
{% endblock %}
