{#
Required params:
- select_name
- selected_value
- selected_label
- default_label
- default_value
- form_id

Either:
- option_template (template path)
or
- options (list of [value, label] items)

Optional:
- header (header text at top of dropdown)
- align ("right")
#}
{% with
   #select,
   #option
   as
   unique_id,
   option_class
%}
<button type="button" class="btn btn-default dropdown-toggle" data-toggle="dropdown">
    {% if default_value2 and selected_value == default_value2 %}
        {{ default_label2 }}
    {% elseif selected_value %}
        {{ selected_label }}
    {% else %}
        {{ default_label }}
    {% endif %}
    <span class="caret"></span>
</button>
<input type="hidden" name="{{ select_name }}" id="{{ unique_id }}" value="{{ selected_value }}" />
<ul class="dropdown-menu{% ifequal align "right" %} dropdown-menu-right{% endifequal %}" role="menu">
    {% if header %}
        <li role="presentation" class="dropdown-header">
            {{ header }}
        </li>
    {% endif %}
    {% if default_label %}
        <li class="{% if not selected_value %}active{% endif %}">
            <a href="#" class="{{ option_class }}" data-value="{{ default_value }}">
                {{ default_label }}
            </a>
        </li>
    {% endif %}
    {% if default_label2 %}
        <li class="{% if selected_value == default_value2 %}active{% endif %}">
            <a href="#" class="{{ option_class }}" data-value="{{ default_value2 }}">
                {{ default_label2 }}
            </a>
        </li>
    {% endif %}
    {% if default_label or default_label2 %}
        <li class="divider"></li>
    {% endif %}
    {% if option_template %}
        {% include option_template
           selected_value=selected_value
           option_class=option_class
        %}
    {% elseif options %}
        {% for value, label in options %}
            <li class="{% ifequal value selected_value %}active{% endifequal %}">
            <a href="#" class="{{ option_class }}" data-value="{{ value }}">{{ label }}</a>
        </li>
        {% endfor %}
    {% endif %}
</ul>
{% javascript %}
    $(".{{ option_class }}").click(function() {
        var form_id = '{{ form_id }}';
        document.getElementById('{{ unique_id }}').value=this.getAttribute('data-value');
        if (form_id) {
            document.getElementById(form_id).submit();
        }
    });
{% endjavascript %}
{% endwith %}