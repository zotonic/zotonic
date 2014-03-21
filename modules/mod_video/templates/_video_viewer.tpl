<video id="{{ #video }}" width="{{ props.width }}" height="{{ props.height }}" poster="{% image_url props.id width=props.width height=props.height %}" controls="controls" preload="none">
    <source type="{{ props.mime }}" src="{% url media_inline star=props.filename %}" />
    <object width="{{ props.width }}" height="{{ props.height }}" type="application/x-shockwave-flash" data="flashmediaelement.swf">
        <param name="movie" value="{% url lib star='me/flashmediaelement.swf' %}" />
        <param name="flashvars" value="controls=true&amp;file={% url media_inline star=props.filename %}" />
        <img src="{% image_url props.id width=props.width height=props.height %}" width="{{ props.width }}" height="{{ props.height }}" title="{_ No video playback capabilities _}" />
    </object>
</video>

{#
<video width="{{ props.width }}" height="{{ props.height }}" 
    controls="controls" preload="none"
    src="{% url media_inline star=props.filename %}"
    type="{{ props.mime }}">
</video>
#}

{% javascript %}
{% endjavascript %}
