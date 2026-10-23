<table class="table table-bordered">
    <thead>
        <tr>
            <th>{_ Module _}</th>
            <th>{_ Template _}</th>
            <th>{_ Line _}</th>
            <th>{_ Extends _}</th>
        </tr>
    </thead>
    <tbody>
        {% for tpl, extends in inheritance_by_file %}
            <tr>
                {% with tpl|development_filename|split:"/priv/templates/" as ref %}
                    <td>
                        {{ ref[1]|escape_check }}
                    </td>
                    <td>
                        {{ ref[2]|escape_check }}
                        {% button text=_"View" class="btn btn-primary btn-xs" postback={template_view template=tpl} delegate=`mod_development` %}
                        {% include "_development_editor_button.tpl" editor_file=tpl editor_line=extends.line editor_column=extends.column editor_label=_"Edit" editor_compact editor_title=_"Edit template" %}
                    </td>
                {% endwith %}
                <td>{{ extends.line }}</td>
                <td>
                    {% if extends.is_missing %}
                        {{ extends.template|escape }} <span class="label label-danger">{_ Missing template _}</span>
                    {% else %}
                        {% include "_development_dispatch_template_option.tpl" template_option=extends.option template_label=extends.template %}
                    {% endif %}
                </td>
            </tr>
        {% empty %}
            <tr>
                <td colspan="4">
                    <span class="text-muted">
                        {_ No references found. _}
                    </span>
                </td>
            </tr>
        {% endfor %}
    </tbody>
</table>
