{% if list %}
{% if title %}<h2>{{ title }}</h2>{% endif %}
<ul class="content-list {{ class }}">
{% for id in list %}
{% if id /= exclude %}
    {% catinclude "_content_list_item.tpl" id counter=forloop.counter exclude=exclude in_collection=in_collection %}
{% endif %}
{% endfor %}
</ul>
{% endif %}
