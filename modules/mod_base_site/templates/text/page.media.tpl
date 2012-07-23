{% extends "page.tpl" %}

{% block below_body %}
    {% inherit %}
    <p class="thumbnail">
        <a href="{% url media_inline id=id %}" title="{_ Click to download _}"><img src="{% image_url id mediaclass="base-media-preview" %}" alt="{_ Preview _}" /></a>
    </p>
    {% include "_media_info.tpl" %}

    {% include "_content_list.tpl" list=id.s.depiction %}
    {% include "_content_list.tpl" list=id.s.hasdocument %}
{% endblock %}
