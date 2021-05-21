{% comment %}
    Used to display media in body texts of emails.
{% endcomment %}
{% if id.is_a.image or id.is_a.video %}
<img src="{% image_url id upscale width=1200 height=600 absolute_url %}" width="600" height="" alt="{{ id.title }}" border="0" style="width: 100%; max-width: 600px; height: auto; background: #dddddd; font-family: sans-serif; font-size: 15px; line-height: 15px; color: #555555; margin: 0 auto 10px auto; display: block;" class="g-img">
{% endif %}
