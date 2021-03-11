{% extends "mailing_page.tpl" %}

{% block below_body %}
	{% for id in id.o.haspart|default:m.search[{query query_id=id pagelen=pagelen|default:20}] %}
		{% if id.is_visible %}
			{% include "email/_spacer.tpl" %}
			{% if id.depiction as depiction %}
		    <tr>
		        <td style="background-color: #ffffff;">
		        	<a href="{{ id.page_url_abs }}">
		            <img src="{% image_url depiction upscale width=1200 height=600 crop absolute_url %}" width="600" height="" alt="{{ id.title }}" border="0" style="width: 100%; max-width: 600px; height: auto; background: #dddddd; font-family: sans-serif; font-size: 15px; line-height: 15px; color: #555555; margin: auto; display: block;" class="g-img">
		        	</a>
		        </td>
		    </tr>
			{% endif %}
			<tr>
			    <td style="background-color: #ffffff;">
			        <table role="presentation" cellspacing="0" cellpadding="0" border="0" width="100%">
			            <tr>
			                <td style="padding: 20px; font-family: sans-serif; font-size: 15px; line-height: 20px; color: #555555;">
			                	<h2><a href="{{ id.page_url }}" style="color: #555555; text-decoration: none;">{{ id.title }}</a></h2>
			                	<p>{{ id|summary:500 }}</p>

			                	<p>&gt; <a style="color: #555555; text-decoration: underline;" href="{{ id.page_url_abs }}">{_ Read more on the web _}</a>
				             </td>
			            </tr>
			        </table>
			    </td>
			</tr>
		{% endif %}
	{% endfor %}
{% endblock %}
