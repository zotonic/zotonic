{% extends "base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}

{% block content %}
<article id="content" class="zp-100">
    <div class="padding">
	<h1>{_ Statistics _}</h1>

        {% lib "js/jquery.flot.js" %}
	{% statistics %}

    </div>
</article>
{% endblock %}


