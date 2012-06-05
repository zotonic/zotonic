{% if id.is_visible and id.is_published and not id|member:exclude %}
<li {% include "_language_attrs.tpl" id=id class="do_clickable" %}>
    <a href="{{ id.page_url with in_collection=in_collection }}">{{ id.title|default:"&mdash;" }}</a>
</li>
{% endif %}
