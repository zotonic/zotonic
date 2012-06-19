{% extends "page.tpl" %}

{% block content %}
    {% inherit %}
    <p class="thumbnail">
        {% media id width=400 %}
    </p>

    {% with id.medium as medium %}
        {% if medium.filename %}
            {% include "_media_info.tpl" %}
        {% elseif medium.video_embed_code %}
            <h2>Embed code</h2>
            <pre>{{ medium.video_embed_code|force_escape|linebreaksbr }}</pre>
        {% endif %}
    {% endwith %}


    {% include "_content_list.tpl" list=id.s.haspart %}
    {% include "_content_list.tpl" list=id.s.depiction %}
    {% include "_content_list.tpl" list=id.s.hasdocument %}
{% endblock %}
