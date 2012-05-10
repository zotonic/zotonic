{% if list %}
{% if title %}<h3>{{ title }}</h3>{% endif %}
<table class="table {{ class }}">
{% for id in list %}
    {% catinclude "_content_list_item.tpl" id counter=forloop.counter %}
{% endfor %}
</table>
{% endif %}
