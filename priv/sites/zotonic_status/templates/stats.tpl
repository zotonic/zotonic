{% extends "base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}

{% block content %}
<article id="content" class="zp-100">
    <div class="padding">
        {% with (q.which|default:'second') as which %}
        <select style="float: right" onchange="window.location='?which='+this.value;">
            <option value='second' {% if which == 'second' %}selected="selected"{% endif %}>Last minute</option>
            <option value='minute' {% if which == 'minute' %}selected="selected"{% endif %}>Last hour</option>
            <option value='hour' {% if which == 'hour' %}selected="selected"{% endif %}>Last day</option>
            <option value='day' {% if which == 'day' %}selected="selected"{% endif %}>Last year</option>
        </select>
        {% endwith %}
        
	<h1>{_ Statistics _}</h1>

        {% lib "js/jquery.flot.js" %}
	{% statistics %}

    </div>
</article>
{% endblock %}


