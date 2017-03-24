{% if id and id.is_visible %}
    {% with id.medium as medium %}
        <entry xmlns="http://www.w3.org/2005/Atom" xmlns:gd="http://schemas.google.com/g/2005" xml:lang="{{ z_language }}">
            <id>{{ id.uri|escapexml }}</id>
            <updated>{{ id.modified|date:"c" }}</updated>
            <published>{{ id.publication_start|date:"c" }}</published>
            <author>{% with id.o.author[1]|default:id.creator_id as author_id %}
                <name>{% filter escapexml %}{% include "_name.tpl" id=author_id %}{% endfilter %}</name>
                <uri>{{ author_id.uri|escapexml }}</uri>
            {% endwith %}</author>
            <link rel="alternate" type="text/html" href="{{ id.page_url_abs }}"/>
            {% if medium.filename %}
                <link rel="enclosure" type="{{ medium.mime }}" href="{% url media_attachment star=medium.filename use_abolute_url %}" />
            {% elseif medium %}
                <link rel="enclosure" type="image/jpeg" href="{% image_url id width=400 height=400 absolute_url %}" />
            {% endif %}
            {% for media_id in id.media %}
                <link rel="enclosure" type="image/jpeg" href="{% image_url media_id width=400 height=400 absolute_url %}" />
            {% endfor %}
            <title>{{ id.title|escapexml }}</title>
            {% if id.body %}
                <summary>{{ id.summary|escapexml }}</summary>
                <content type="html">{{ id.body|escapexml }}</content>
            {% else %}
                <content type="text">{{ id.summary|escapexml }}</content>
            {% endif %}
            {% if medium.filename %}
                <content type="{{ medium.mime }}" src="{% url media_attachment star=medium.filename absolute_url %}" />
            {% endif %}
            {% if id.is_a.event and id.date_start %}
                {# http://code.google.com/apis/gdata/docs/2.0/elements.html#gdEventKind #}
                <category scheme="http://schemas.google.com/g/2005#kind" term="http://schemas.google.com/g/2005#event" />
                <gd:eventStatus value="http://schemas.google.com/g/2005#event.confirmed" />
                <gd:when startTime="{{ id.date_start|date:"c" }}" endTime="{{ id.date_end|date:"c" }}" />
                {# <gd:where>My Living Room</gd:where> #}
            {% endif %}

            <category term="{{ id.category.name }}" scheme="http://zotonic.com/id/category" />
        </entry>
    {% endwith %}
{% endif %}
