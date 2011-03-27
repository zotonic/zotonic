{% extends "admin_widget_dashboard.tpl" %}

{# Renders latest modified persons dashboard widget #}

{% block widget_headline %}
	{_ Latest modified persons _}
	{% button class="right" action={redirect dispatch="admin_overview_rsc" qcat="person"} text=_"show all"%}
{% endblock %}

{% block widget_content %}
<ul class="short-list">
	<li class="headers clearfix">
		<span class="zp-55">{_ Title _}</span>
		<span class="zp-25">{_ Category _}</span>
		<span class="zp-20">{_ Options _}</span>
	</li>

	{% for id in m.search[{latest cat="person" pagelen="5"}] %}
	    {% if m.rsc[id].is_visible %}
		<li {% if not m.rsc[id].is_published %}class="unpublished" {% endif %}>
		    <a href="{% url admin_edit_rsc id=id %}" class="clearfix">
			<span class="zp-55">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
			<span class="zp-25">{{ m.rsc[m.rsc[id].category_id].title }}</span>
			<span class="zp-20">
			    {% button text=_"view" action={redirect id=id} %}
			    {% button text=_"edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
			</span>
		    </a>
		</li>
	    {% endif %}

	{% empty %}
	    <li>
		{_ No persons. _}
	    </li>
	{% endfor %}
</ul>
{% endblock %}
