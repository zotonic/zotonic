{% with m.rsc[id] as r %}
	{% with r.medium as medium %}
		<entry xmlns="http://www.w3.org/2005/Atom" xmlns:gd="http://schemas.google.com/g/2005" xml:lang="en">
			<id>{{ r.uri|escapexml }}</id>
			<updated>{{ r.modified|date:"c" }}</updated>
			<published>{% if upcoming %}{{ r.date_start|date:"c" }}{% else %}{{ r.publication_start|date:"c" }}{% endif %}</published>
			<author>
				{% with r.o.author[1] as author_id %}
				<name>{{ m.rsc[author_id|default:r.creator_id].title|escapexml }}</name>
				<uri>{{ m.rsc[author_id|default:r.creator_id].uri|escapexml }}</uri>
				{% endwith %}
			</author>
			
			<link rel="alternate" type="text/html" href="{{ site_url }}{{ r.page_url|escapexml }}"/>
			
			{% if medium.filename %}
				<link rel="enclosure" type="{{ medium.mime }}" href="{{ site_url }}{% url media_attachment star=medium.filename %}" />
			{% endif %}
			
			{% for media_id in r.media %}
				<link rel="enclosure" type="image/jpeg" href="{{ site_url }}{% image_url media_id width=400 height=400 %}" />
			{% endfor %}

			<title>{{ r.title }}</title>
			{% if r.body %}
				<summary>{{ r.summary }}</summary>
				<content type="html">{{ r.body|escapexml }}</content>
			{% else %}
				<content type="text">{{ r.summary|escapexml }}</content>
			{% endif %}
			{% if medium.filename %}
				<content type="{{ medium.mime }}" src="{{ site_url }}{% url media_attachment star=medium.filename %}" /> 
			{% endif %}
			
			{% if r.is_a.event and r.date_start %}
				{# http://code.google.com/apis/gdata/docs/2.0/elements.html#gdEventKind #}
				<category scheme="http://schemas.google.com/g/2005#kind" term="http://schemas.google.com/g/2005#event" />
				<gd:eventStatus value="http://schemas.google.com/g/2005#event.confirmed" />
				<gd:when startTime="{{ r.date_start|date:"c" }}" endTime="{{ r.date_end|date:"c" }}" />
				{# <gd:where>My Living Room</gd:where> #}
			{% endif %}
				
			<category term="{{ r.category.name }}" scheme="http://zotonic.com/id/category" />
		</entry>
	{% endwith %}
{% endwith %}
