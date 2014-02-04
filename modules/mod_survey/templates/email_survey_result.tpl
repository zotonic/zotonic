{% extends "email_base.tpl" %}

{% block title %}{_ New survey result: _} {{ id.title }}{% endblock %}

{% block body %}

<p>{_ The following survey has been filled in: _} <a href="{{ id.page_url_abs }}">{{ id.title }}</a></p>

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
    	{% if ans|is_list %}
	  		{% for v in ans %}
	       		{% if v /= "" %}{{ v|force_escape|linebreaksbr }}{% if not forloop.last %}<br/>{% endif %}{% endif %}
		    {% endfor %}
		{% else %}
			{{ ans|force_escape|linebreaksbr }}
		{% endif %}
       	</td>
    </tr>
{% endfor %}
</table>

{% block edit_answer %}
	<p><a href="{% url admin_edit_rsc id=id use_absolute_url %}">{_ Check the answer in the admin. _}</a></p>
{% endblock %}

{% endblock %}
