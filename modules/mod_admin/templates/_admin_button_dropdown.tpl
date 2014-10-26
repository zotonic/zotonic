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
#}
{% with
   #select,
   #option
   as
   unique_id,
   option_class
%}
<button type="button" class="btn btn-default dropdown-toggle" data-toggle="dropdown">
    {% if selected_value %}
        {{ selected_label }}
    {% else %}
        {{ default_label }}
    {% endif %}
    <span class="caret"></span>
</button>
<input type="hidden" name="{{ select_name }}" id="{{ unique_id }}" value="{{ selected_value }}" />
<ul class="dropdown-menu dropdown-menu-right" role="menu">
    {% if header %}
        <li role="presentation" class="dropdown-header">
            {{ header }}
        </li>
    {% endif %}
    <li class="{% if not selected_value %}active{% endif %}">
        <a href="#" class="{{ option_class }}" data-value="{{ default_value }}">
            {{ default_label }}
        </a>
    </li>
    <li class="divider"></li>
    {% if option_template %}
        {% include option_template
           selected_value=selected_value
           option_class=option_class
        %}
    {% elif options %}
        {% for value, label in options %}
            <li class="{% ifequal value selected_value %}active{% endifequal %}">
            <a href="#" class="{{ option_class }}" data-value="{{ value }}">{{ label }}</a>
        </li>
        {% endfor %}
    {% endif %}
</ul>
{%
    wire 
    id=" ." ++ option_class
    action={
        script
        script="
            document.getElementById('" ++ unique_id ++ "').value=this.getAttribute('data-value');
            document.getElementById('" ++ form_id ++ "').submit();
        "
    }
%}
{% endwith %}