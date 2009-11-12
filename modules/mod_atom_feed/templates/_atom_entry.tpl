{% with m.rsc[id] as r %}
	{% with r.medium as medium %}
		<entry xmlns="http://www.w3.org/2005/Atom" xml:lang="en">
			<id>{{ r.resource_uri }}</id>
			<updated>{{ r.modified|date:"c" }}</updated>
			<author>
				{% with r.o.author[1] as author_id %}
				<name>{{ m.rsc[author_id|default:r.creator_id].title|escapexml }}</name>
				<uri>{{ m.rsc[author_id|default:r.creator_id].resource_uri|escapexml }}</uri>
				{% endwith %}
			</author>
			
			<link rel="alternate" type="text/html" href="{{ site_url }}{{ r.page_url|escapexml }}"/>
			
			{% if medium.filename %}
				<link rel="enclosure" type="{{ medium.mime }}" src="{{ site_url }}{% url media_attachment star=medium.filename %}" />
			{% endif %}
			
			{% for media_id in r.media %}
				<link rel="enclosure" type="image/jpeg" src="{{ site_url }}{% image_url media_id width=400 height=400 %}" />
			{% endfor %}

			<title>{{ r.title|escapexml }}</title>
			<summary>{{ r.intro|escapexml }}</summary>
			<content type="text/html">{{ r.body|escapexml }}</content>
			{% if medium.filename %}
				<content type="{{ medium.mime }}" src="{{ site_url }}{% url media_attachment star=medium.filename %}" /> 
			{% endif %}
			<category term="{{ r.category.name }}" scheme="http://zotonic.com/id/category" />
		</entry>
	{% endwith %}
{% endwith %}
