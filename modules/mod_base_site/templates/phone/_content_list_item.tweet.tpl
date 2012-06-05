{% if id.is_visible and id.is_published and not id|member:exclude %}
<li {% include "_language_attrs.tpl" id=id %}>
    {% with id.depiction as dep %}
    {% if dep %}
    <img src="{% image_url dep mediaclass="base-list-item-small" %}" alt="" /> 
    {% endif %}
    {% endwith %}
    <h3>{{ m.rsc.tweet.title }}</h3>
    {{ id.body }}
</li>
{% endif %}
