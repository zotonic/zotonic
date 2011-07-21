{% extends "admin_widget_dashboard.tpl" %}

{# 
 Display latest modified rscs

 Required arguments:
  cat= rsc category
  headline= block caption

 Optional:
  last= true|false default false
  pagelen= page length, default 5

#}

{% block widget_headline %}
    {{ headline }}
    {% button class="right" action={redirect dispatch="admin_overview_rsc" qcat=cat} text=_"show all"%}
{% endblock %}

{% block widget_class %}{% if last %}last{% endif %}{% endblock %}

{% block widget_content %}
<ul class="short-list">
	<li class="headers clearfix">
		<span class="zp-55">{_ Title _}</span>
		<span class="zp-45">{_ Category _}</span>
	</li>

	{% for id in m.search[{latest cat=cat pagelen=pagelen|default:5}] %}
	    {% if m.rsc[id].is_visible %}
		<li class="clearfix{% if not m.rsc[id].is_published %} unpublished{% endif %}">
		    <a href="{% url admin_edit_rsc id=id %}" class="row">
                {% if media %}
                <span class="zp-15">{% image id width=40 height=18 crop %}&nbsp;</span>
                <span class="zp-40">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
                {% else %}
                <span class="zp-55">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
                {% endif %}
                <span class="zp-45">{{ m.rsc[m.rsc[id].category_id].title }}</span>
		    </a>
			<span class="button-area">
                <a href="{{ m.rsc[id].page_url }}" class="button">{_ view _}</a>
                <a href="{% url admin_edit_rsc id=id %}" class="button">{_ edit _}</a>
			</span>
		</li>
	    {% endif %}

	{% empty %}
	    <li>
		{_ No items. _}
	    </li>
	{% endfor %}
</ul>
{% endblock %}
