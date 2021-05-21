{#
    Video viewer template.

    Used for {% media %} by mod_video to display media items with
    the mime type "video/mp4"
#}
{% with props|default:(m.media[id]) as props %}
{% if props.is_video_processing %}
	<div id="{{ #video }}" class="video-processing">
		<img src="/lib/images/processing.gif" />
		<span>{_ Converting _} …</span>
	</div>
    {% wire type={mqtt topic=[ "bridge", "origin", "model", "media", "event", props.id, "update" ]}
            action={replace template="_video_viewer.tpl" target=#video options=options id=props.id}
    %}
{% elseif props.is_video_broken %}
	{% image props.preview_filename width=options.width height=options.height class=options.class %}
{% elseif props.filename %}
	<video id="{{ #video }}" style="width: 100%; height: auto;"
            {% if props.width and props.height %}
                poster="{% image_url props.id width=props.width height=props.height %}"
            {% else %}
                poster="{% image_url props.id width=800 height=450 %}"
            {% endif %}
			controls
            {% if options.autoplay %}autoplay
            {% else %}preload="none"
            {% endif %}
            {% if props.width and props.height %}
                width="{{ props.width }}" height="{{ props.height }}"
            {% else %}
                width="800" height="450"
            {% endif %}
        >
		<source type="{{ props.mime }}" src="{% url media_inline star=props.filename %}" />
	</video>
{% endif %}
{% endwith %}
