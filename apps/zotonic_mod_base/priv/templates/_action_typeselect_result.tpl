{% for id, rank in result %}
    <li class="suggestions-result clearfix">
        <a id="{{ #connect.id }}" href="#add-connection">
            <span class="pull-right">{{ id.category.id.title|default:id.category.name }}</span>
            {% image id.depiction width=40 height=18 crop %}
            {{ id.title }}
        </a>
    </li>

    {% wire_args id=#connect.id action=action_with_id select_id=id %}
    {% wire id=#connect.id action=action %}
{% empty %}
    <li class="suggestions-result"><a href="#">Nothing found.</a></li>
{% endfor %}
