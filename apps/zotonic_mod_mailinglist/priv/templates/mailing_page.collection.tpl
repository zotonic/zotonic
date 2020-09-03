{% extends "mailing_page.tpl" %}

{# Main body of the message sent. #}
{% block content %}
{% with m.mailinglist.subscription[list_id][email] as sub %}
<tr>
	<td id="content">
		{% image id.depiction width=200 height=300 align="left" style="float: left; margin: 0 16px 16px 0; padding: 0; border: 0;" %}
		<h1>{{ id.title }}</h1>
		{% if id.summary %}
			<p><b>{{ id.summary }}</b></p>
		{% endif %}
		{{ id.body|show_media:"_body_media_mailing.tpl"|inject_recipientdetails:sub }}
		{% include "_blocks.tpl" %}
	</td>
</tr>
<tr>
	<td>
		<table width="100%" border="0" cellspacing="0" cellpadding="0">
		{% for pid in id.o.haspart|default:m.search[{query query_id=id}] %}
		{% if pid.depiction %}
		<tr>
			<td width="150" valign="top">
				<a href="{{ pid.page_url_abs }}">{% image pid.depiction width=150 style="border: 0"%}</a>
			</td>
			<td width="15">&nbsp;</td>
			<td valign="top">
				<h2><a href="{{ pid.page_url_abs }}">{{ pid.title }}</a></h2>
				<p>{{ pid|summary }}</p>
			</td>
		</tr>
		{% else %}
		<tr>
			<td colspan="3" valign="top">
				<h2><a href="{{ pid.page_url_abs }}">{{ pid.title }}</a></h2>
				<p>{{ pid|summary }}</p>
			</td>
		</tr>
		{% endif %}
		{% endfor %}
		</table>
		<br/>
	    <p><a href="{{ id.page_url_abs }}">{_ Read this page on the web. _} &raquo;</a></p>
	</td>
</tr>
{% endwith %}
{% endblock %}
