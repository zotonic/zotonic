{% extends "admin_widget_dashboard.tpl" %}

{# Renders admin dashboard widget. 5 latest resources of category "mycat" will be displayed. #}


{# Headline block. Caption, some buttons, etc #}
{% block widget_headline %}
	{_ My stuff _}
	
	{# A button may be displayed at right corner of widget heading. #}
	{% button class="right" action={redirect dispatch="admin_overview_rsc" qcat="mycat"} text=_"show all" %}
{% endblock %}


{# Additional css-class for container. Optional. #}
{% block widget_class %}last{% endblock %}


{# Your data goes here. For example, it could be usual zotonic admin listing: #}
{% block widget_content %}
    <ul class="short-list">
	<li class="headers clearfix">
		<span class="zp-5">#</span>
		<span class="zp-65">{_ Title _}</span>
		<span class="zp-25">{_ Actions _}</span>
	</li>

	{% for id in m.search[{latest cat="mycat" pagelen="5"}] %}

		<li {% if not m.rsc[id].is_published %}class="unpublished" {% endif %}>
		    <a href="{% url admin_edit_rsc id=id %}" class="clearfix">
			<span class="zp-5">{% forloop.counter %}</span>
			<span class="zp-65">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
			<span class="zp-25">
			    {% button text=_"view" action={redirect id=id} %}
			    {% button text=_"edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
			</span>
		    </a>
		</li>

	{% empty %}
	    <li>
		{_ No stuff. _}
	    </li>
	{% endfor %}
    </ul>
{% endblock %}
