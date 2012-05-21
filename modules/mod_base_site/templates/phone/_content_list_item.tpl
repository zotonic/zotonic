{% if id.is_visible and id.is_published and not id|member:exclude %}
<li {% include "_language_attrs.tpl" id=id class="do_clickable" %}>
    {% with id.depiction as dep %}
    {% if dep %}
    <img src="{% image_url dep mediaclass="base-list-item-small" %}" alt="" /> 
    {% endif %}
    {% endwith %}
    <h2>{{ id.title|default:"&mdash;" }}</h2>
	<p>    
		{% if id.summary %}
		{{id.summary|truncate:120}}
    	{% elseif id.body %}
		{{ id.body|striptags|truncate:120 }}
		{% endif %}
		<a href="{{ id.page_url with in_collection=in_collection }}">{_ Read more _} &raquo;</a>
	</p>

    
</li>
{% endif %}
