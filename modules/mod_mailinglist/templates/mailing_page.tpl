{% extends "email_base.tpl" %}

{% block title %}{{ id.title }}{% endblock %}

{% block body %}
<p style="text-align: center"><a href="http://{{ m.site.hostname }}{{ id.page_url }}">{_ Click here when you can’t read the message below. _}</a></p>

<hr/>

<h1>{{ id.title }}</h1>

{% if id.body %}
	{% if id.summary %}
		<p><strong>{{ id.summary|linebreaksbr }}</strong></p>
	{% endif %}
	{{ id.body }}
{% else %}
	<p>{{ id.summary|linebreaksbr }}</p>
{% endif %}

<p><a href="http://{{ m.site.hostname }}{{ m.rsc[id].page_url }}">{_ Read this page on the web. _}</a></p>

{% include "_mailing_footer.tpl" %}
{% endblock %}
