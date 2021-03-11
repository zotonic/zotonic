{% extends "mailing_page.tpl" %}

{# Main body of the message sent. #}
{% block depiction %}
    <tr>
        <td style="background-color: #ffffff;">
            <img src="{% image_url id upscale width=1200 absolute_url %}" width="600" height="" alt="{{ id.depiction.title }}" border="0" style="width: 100%; max-width: 600px; height: auto; background: #dddddd; font-family: sans-serif; font-size: 15px; line-height: 15px; color: #555555; margin: auto; display: block;" class="g-img">
        </td>
    </tr>
{% endblock %}
