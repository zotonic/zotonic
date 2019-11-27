{% extends "base.tpl" %}

{% block title %}{{ m.site.title }}{% endblock %}

{% block main %}

<div class="jumbotron">
    {% button class="pull-right btn btn-primary btn-lg" action={redirect dispatch=`admin`} text=_"Visit Admin Interface" %}
    <h1>{{ m.rsc.page_home.title }}</h1>
    <p>{{ m.rsc.page_home.summary }}</p>
</div>

{{ m.rsc.page_home.body|show_media }}

{% button class="btn btn-info" action={redirect dispatch=`admin_edit_rsc` id=`page_home`} text=_"Edit this page" %}

{% endblock %}

{% block subnavbar %}
{% include "_content_list.tpl" list=m.search[{query sort='-rsc.modified' pagelen=10}] title=_"Recent content" %}
{% endblock %}
