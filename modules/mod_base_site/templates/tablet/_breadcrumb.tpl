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
{% endif %}
{% endwith %}

