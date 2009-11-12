{% if medium.filename %}
	<p>
		{{ medium.mime }} 
		&mdash; {{ medium.width }} x {{ medium.height }} pixels
		&mdash; {{ medium.filename }}
		&mdash; uploaded on {{ medium.created|date:"Y-m-d H:i:s" }}
	</p>
	<div class="edit_media">
	{% if medium.width|lt:606  %}
		{% media medium %}
	{% else %}
		{% media medium width=606 height=606 %}
	{% endif %}
	</div>
	<div>
		{% button text="download" action={redirect dispatch="media_attachment" star=medium.filename} %}
	</div>
{% endif %}