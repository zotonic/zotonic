{# Menu for text #}
<ul class="nav breadcrumb">
{% with id|menu_subtree:menu_id as subtree %}
{% with id|menu_trail:menu_id as breadcrumb %}
    {% for pid in breadcrumb %}
    {% if subtree or not forloop.last %}
        <li><a class="item-{{ forloop.counter0 }}" href="{{ pid.page_url }}">{{ pid.short_title|default:(pid.title) }} <span class="divider">/</span></a></li>
    {% endif %}
    {% endfor %}
    {% if subtree %}
        <ul class="nav">
        {% with breadcrumb|length as depth %}
        {% for pid,_m in subtree %}
            <li><a class="item-{{ depth }}" href="{{ pid.page_url }}">{{ pid.short_title|default:(pid.title) }}</a></li>
        {% endfor %}
        {% endwith %}
        </ul>
    {% elseif not breadcrumb %}
        <ul class="nav">
        {% for pid,_m in menu_id.menu %}
            <li><a href="{{ pid.page_url }}">{{ pid.short_title|default:(pid.title) }}</a></li>
        {% endfor %}
        </ul>
    {% endif %}
{% endwith %}
{% endwith %}
</ul>
