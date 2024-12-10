{% if id.is_editable %}
    <div class="clearfix">
        <div class="block-add-block">
            <a class="btn btn-default dropdown-toggle" data-toggle="dropdown" href="#">
                {_ + add page block _}
                <span class="caret"></span>
            </a>
            <ul class="dropdown-menu">
                {% for _order, title, items in blocks %}
                    {% if title %}
                        <li class="dropdown-header">{{ title }}</li>
                    {% endif %}
                    {% for type, title in items %}
                        <li>
                            <a href="#" data-block-type="{{ type }}">
                            {% with title|split:"|" as ts %}
                                {% if ts[2] %}
                                    {{ ts[1] }} <span class="text-muted">{{ ts[2 ]}}</span>
                                {% else %}
                                    {{ ts[1] }}
                                {% endif %}
                            {% endwith %}
                            </a>
                        </li>
                    {% endfor %}
                {% endfor %}
            </ul>
        </div>
    </div>
{% endif %}
