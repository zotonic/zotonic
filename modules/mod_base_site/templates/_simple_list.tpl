{% if list %}
{% if title %}<h2>{{ title }}</h2>{% endif %}
<ul class="simple-list {{ class }}">
{% for id in list %}
    {% catinclude "_simple_list_item.tpl" id counter=forloop.counter exclude=exclude in_collection=in_collection %}
{% endfor %}
</ul>
{% endif %}