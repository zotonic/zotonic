<nav class="trail">
{% with id|menu_trail as breadcrumb %}
{% if breadcrumb %}
    <ul class="breadcrumb">
        {% for id in breadcrumb %}
            {% if forloop.last %}
            <li class="active">{{ id.short_title|default:id.title }}</li>
            {% else %}
            <li><a href="{{ id.page_url }}">{{ id.short_title|default:id.title }}</a> <span class="divider">/</span></li>
            {% endif %}
        {% endfor %}
    </ul>
{% else %}
	{% with q.in_collection|default:id.s.haspart[1] as in_collection %}
	{% if in_collection %}
	{% with m.rsc[in_collection].o.haspart as ps %}
	    {% for p in ps %}
	    {% if p == id %}
	        {% with ps[forloop.counter-1], ps[forloop.counter+1] as prev, next %}
	        <ul class="breadcrumb">
	            <li><a href="{{  m.rsc[in_collection].page_url }}">{{ m.rsc[in_collection].short_title|default:m.rsc[in_collection].title }}</a> <span class="divider">/</span></li>
	            <li class="active">
		            {% if prev %}<a href="{% url page id=prev in_collection=in_collection %}">&larr; </a>{% endif %}
					{{ id.short_title|default:id.title }}
		            {% if next %}<a href="{% url page id=next in_collection=in_collection %}">&rarr;</a>{% endif %}
				</li>
	        </ul>
	        {% endwith %}
	    {% endif %}
	    {% endfor %}
	{% endwith %}
	{% endif %}
	{% endwith %}
{% endif %}
{% endwith %}
</nav>
