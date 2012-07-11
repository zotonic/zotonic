{% if is_editable %}
<div class="btn-group pull-right block-add-block">
    <a class="btn btn-mini dropdown-toggle" data-toggle="dropdown" href="#">
        {_ + add block _}
        <span class="caret"></span>
    </a>
    <ul class="dropdown-menu nav-list nav">
        {% for _order, title, items in blocks %}
            {% if title %}
            <li class="nav-header">{{ title }}</li>
            {% endif %}
            {% for type, title in items %}
                <li><a href="#" data-block-type="{{ type }}">{{ title }}</li></a>
            {% endfor %}
        {% endfor %}
    </ul>
</div>
{% endif %}
