{% if menu %}
{% with id|menu_trail as parents %}
    <ul id="{{ id_prefix }}navigation" class="clearfix at-menu do_superfish">
    {% for mid,depth,nr,has_sub in menu %}
		{% if not mid %}{% if depth > 1 %}</ul></li>{% endif %}
		{% else %}
        	{% if nr == 1 and not forloop.first %}<ul{% if mid|member:path %} class="onpath"{% endif %}>{% endif %}
	        <li id="{{ id_prefix }}nav-item-{{nr}}" 
	            class="{% if is_first %}first {% endif %}{% if is_last %}last{% endif %}">
	            <a href="{{ m.rsc[mid].page_url }}" 
	               class="{{ m.rsc[mid].name }}{% if mid|member:parents %} current{% else %}{% if mid|member:path %} onpath{% endif %}{% endif %}">{{ m.rsc[mid].short_title|default:m.rsc[mid].title }}</a>
			{% if not has_sub %}</li>{% endif %}
		{% endif %}
    {% endfor %}
    {% if forloop.last %}{% include "_menu_extra.tpl" %}{% endif %}
    </ul>
{% endwith %}
{% endif %}
