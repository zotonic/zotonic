{% with props|default:(m.media[id]) as props %}
{% if props.is_video_processing %}
	<div id="{{ #video }}" class="video-processing">
		<img src="/lib/images/processing.gif" />
		<span>{_ Converting _} …</span>
	</div>
    {% javascript %}
        {% wire type={mqtt topic=["model", "media", "event", props.id]}
                action={replace template="_video_viewer.tpl" target=#video options=options id=props.id}
        %}
    {% endjavascript %}
{% elseif props.is_video_broken %}
	{% image props.preview_filename width=options.width height=options.height class=options.class %}
{% elseif props.filename %}
	<video id="{{ #video }}" style="max-width: 100%; height: auto;" poster="{% image_url props.id width=props.width height=props.height %}"
			controls="controls" preload="none"
			width="{{ props.width }}" height="{{ props.height }}">
		<source type="{{ props.mime }}" src="{% url media_inline star=props.filename %}" />
	</video>
{% endif %}
{% endwith %}
