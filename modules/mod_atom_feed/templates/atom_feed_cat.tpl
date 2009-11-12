<feed xmlns="http://www.w3.org/2005/Atom" xml:base="{{ site_url }}" xml:lang="en">
	<generator uri="http://zotonic.com/">Zotonic - Atom Feed Module</generator>
	<updated>{{ updated|date:"c" }}</updated>
	<logo />
	<link rel="self" type="application/atom+xml" href="{{ site_url }}{% url atom_feed cat=cat %}" />
	<id>{{ site_url }}{% url atom_feed cat=cat %}</id>
	<title>{{ m.rsc.home.title|escapexml }} - {{ m.rsc[cat].title|escapexml }}</title>

	{% for id in m.search[{latest cat=cat}] %}
		{% include "_atom_entry.tpl" id=id %}
	{% endfor %}
	
</feed>
