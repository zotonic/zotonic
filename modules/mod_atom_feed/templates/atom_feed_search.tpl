<feed xmlns="http://www.w3.org/2005/Atom" xml:base="{{ site_url }}" xml:lang="en">
	<generator uri="http://zotonic.com/">Zotonic - Atom Feed Module</generator>
	<updated>{{ updated|date:"c" }}</updated>
	<logo />
	<link rel="self" type="application/atom+xml" href="{{ site_url }}{% url atom_feed_search %}" />
	<id>zotonic.atom.search</id>
	<title>{% if qtext %}Latest updates for &quot;{{ qtext }}&quot;{% else %}Latest updates{% endif %}</title>

	{% for id in ids %}
		{% include "_atom_entry.tpl" id=id %}
	{% endfor %}
	
</feed>
