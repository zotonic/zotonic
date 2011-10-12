{% extends "email_base.tpl" %}

{% block title %}{{ id.title }}{% endblock %}

{% block body %}

{% if m.rsc[id].is_published and not m.rsc[id].publication_start|in_future %}
<p style="text-align: center"><a href="http://{{ m.site.hostname }}{{ id.page_url }}">{_ Click here when you can’t read the message below. _}</a></p>
<hr/>
{% endif %}

<h1>{{ id.title }}</h1>

{% if id.body %}
	{% if id.summary %}
		<p><strong>{{ id.summary|linebreaksbr }}</strong></p>
	{% endif %}
	{{ id.body }}
{% else %}
	<p>{{ id.summary|linebreaksbr }}</p>
{% endif %}

{% if m.rsc[id].is_published and not m.rsc[id].publication_start|in_future %}
<p><a href="http://{{ m.site.hostname }}{{ m.rsc[id].page_url }}">{_ Read this page on the web. _}</a></p>
{% endif %}

{% if not m.rsc[list_id].mailinglist_private %}
{% include "_mailing_footer.tpl" %}
{% endif %}

{% endblock %}
