{% extends "email_base.tpl" %}

{% block title %}{_ Test email from _} {{ m.site.hostname }}{% endblock %}

{% block body %}
<p><br></p>
<p>Hi,<br>
<br>
This is a test email that was sent from the site at https://{{ m.site.hostname }}/.<br>
It was requested by {% include "_name.tpl" id=m.acl.user %}
</p>
{% endblock %}

{% block footer %}
{% endblock %}
