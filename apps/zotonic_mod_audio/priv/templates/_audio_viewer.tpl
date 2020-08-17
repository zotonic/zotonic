{% with props|default:(m.media[id]) as props %}
{% if props.filename %}
    <audio id="{{ #audio }}" style="width: 100%; height: auto;"
            controls
            {% if options.autoplay %}autoplay
            {% else %}preload="none"
            {% endif %}
            {% if props.width and props.height %}
                width="{{ props.width }}" height="{{ props.height }}"
            {% else %}
                width="800" height="48"
            {% endif %}
        >
        <source type="{{ props.mime }}" src="{% url media_inline star=props.filename %}" />
    </audio>
{% endif %}
{% endwith %}
