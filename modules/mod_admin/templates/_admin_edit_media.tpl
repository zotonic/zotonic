{% if medium.filename %}
	<p>
		{{ medium.mime }} 
        {% if medium.width and medium.height %}
		&mdash; {{ medium.width }} x {{ medium.height }} pixels
        {% endif %}
		&mdash; {{ medium.filename }}
		&mdash; uploaded on {{ medium.created|date:"Y-m-d H:i:s" }}
	</p>
    {% if medium.width and medium.height %}
    <div class="edit_media">
	{% if medium.width|lt:606  %}
		{% media medium %}
	{% else %}
		{% media medium width=606 height=606 %}
	{% endif %}
	</div>
	{% endif %}
	<div>
		{% button text="download" action={redirect dispatch="media_attachment" star=medium.filename} %}
	</div>
{% endif %}
