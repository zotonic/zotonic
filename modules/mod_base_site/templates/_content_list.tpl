{% if list %}
{% if title %}<h2>{{ title }}</h2>{% endif %}
<ul class="content-list {{ class }}">
{% for id in list %}
    {% catinclude "_content_list_item.tpl" id counter=forloop.counter exclude=exclude in_collection=in_collection is_large=is_large %}
{% endfor %}
</ul>
{% endif %}
