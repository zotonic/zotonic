{% if medium.filename %}
	<p>
		{{ medium.mime }} 
        {% if medium.width and medium.height %}
		&mdash; {{ medium.width }} x {{ medium.height }} {_ pixels _}
        {% endif %}
		&mdash; {{ medium.filename }}
		&mdash; {_ uploaded on _} {{ medium.created|date:"Y-m-d H:i:s" }}
	</p>
    {% if medium.width and medium.height %}
    <div class="edit_media">
	{% if medium.width < 606  %}
		{% media medium %}
	{% else %}
		{% media medium width=606 height=606 %}
	{% endif %}
	</div>
	{% endif %}
	<div>
		<a href="{% url media_attachment star=medium.filename %}" class="button">{_ download _}</a>
	</div>
{% endif %}
