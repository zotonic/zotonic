{% extends "email_base.tpl" %}

{% block title %}{_ New survey result: _} {{ id.title }}{% endblock %}

{% block body %}

<p>{_ The following survey has been filled in: _} <a href="http://{{ m.site.hostname }}{{ id.page_url }}">{{ id.title }}</a></p>

<table>
	<tr>
		<th>{_ Question _}</th>
		<th>{_ Answer _}</th>
	</tr>
{% for name,ans in answers %}
    <tr>
        <td>
        	<strong>{{ name|escape }}</strong>
       	</td>
       	<td>
       		{{ ans|force_escape|linebreaksbr }}
       	</td>
    </tr>
{% endfor %}
</table>

<p><a href="http://{{ m.site.hostname }}{% url admin_edit_rsc id=id %}">{_ Check the answer in the admin. _}</a></p>

{% endblock %}
