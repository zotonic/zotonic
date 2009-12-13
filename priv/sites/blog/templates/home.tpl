{% extends "base.tpl" %}

{% block title %}Welcome to your blog{% endblock %}

{% block featured %}
<div class="featured">
    <h1>Welcome on your blog.</h1>
    <div>Your subtitle should go here...</div>
</div>
{% endblock %}

{% block content %}

{% with m.search.paged[{latest cat='text' page=q.page pagelen=m.config.site.pagelen.value}] as result %}
{% for id in result %}
{% include "_article_summary.tpl" id=id %}
{% endfor %}
{% pager result=result dispatch='home' %}
{% endwith %}

{% endblock %}
