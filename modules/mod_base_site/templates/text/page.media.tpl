{% extends "page.tpl" %}

{% block content %}
    {% inherit %}
    {% include "_media_info.tpl" %}

    <p class="thumbnail">
        <a href="{% url media_inline id=id %}" title="{_ Click to download _}"><img src="{% image_url id mediaclass="base-media-preview" %}" alt="{_ Preview _}" /></a>
    </p>

    {% include "_content_list.tpl" list=id.s.haspart %}
    {% include "_content_list.tpl" list=id.s.depiction %}
{% endblock %}
