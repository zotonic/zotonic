{% with props|default:(m.media[id]) as props %}
{% if props.is_video_processing %}
	<div id="{{ #video }}" class="video-processing">
		<img src="/lib/images/processing.gif" />
	</div>
	{% javascript %}
		{% wire action={connect 
					signal={medium_update id=props.id} 
					action={replace template="_video_viewer.tpl" target=#video options=options id=props.id}
			}
		%}
	{% endjavascript %}
{% elseif props.is_video_broken %}
	{% image props.preview_filename width=options.width height=options.height class=options.class %}
{% elseif props.filename %}
	<video id="{{ #video }}" style="max-width: 100%; height: auto;" poster="{% image_url props.id width=props.width height=props.height %}"
			controls="controls" preload="none"
			width="{{ props.width }}" height="{{ props.height }}">
		<source type="{{ props.mime }}" src="{% url media_inline star=props.filename %}" />
		<object style="max-width: 100%; height: auto;" 
				width="{{ props.width }}" height="{{ props.height }}" 
				type="application/x-shockwave-flash" 
				data="flashmediaelement.swf">
			<param name="movie" value="{% url lib star='me/flashmediaelement.swf' %}" />
			<param name="flashvars" value="controls=true&amp;file={% url media_inline star=props.filename %}" />
			<img src="{% image_url props.id width=props.width height=props.height %}" width="{{ props.width }}" height="{{ props.height }}" title="{_ No video playback capabilities _}" />
		</object>
	</video>
	{#
		MW - mediaelementjs is confused by the video's size, disable for now.
		{% javascript %}
			$('#{{ #video }}').mediaelementplayer();
		{% endjavascript %}
	#}
{% endif %}
{% endwith %}
