{% if list %}
{% if title %}<h3>{{ title }}</h3>{% endif %}
<ul class="unstyled content-list {{ class }}">
{% for id in list %}
    {% catinclude "_content_list_item.tpl" id counter=forloop.counter exclude=exclude %}
{% endfor %}
</ul>
{% endif %}
