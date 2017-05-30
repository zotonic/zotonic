{% if id.is_editable %}
    <div class="clearfix">
        <div class="pull-right block-add-block">
            <a class="btn btn-default btn-xs dropdown-toggle" data-toggle="dropdown" href="#">
                {_ + add block _}
                <span class="caret"></span>
            </a>
            <ul class="dropdown-menu dropdown-menu-right">
                {% for _order, title, items in blocks %}
                    {% if title %}
                        <li class="dropdown-header">{{ title }}</li>
                    {% endif %}
                    {% for type, title in items %}
                        <li><a href="#" data-block-type="{{ type }}">{{ title }}</li></a>
                    {% endfor %}
                {% endfor %}
            </ul>
        </div>
    </div>
{% endif %}
