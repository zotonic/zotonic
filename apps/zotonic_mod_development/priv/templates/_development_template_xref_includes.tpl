<table class="table table-bordered">
    <thead>
        <tr>
            <th>{_ Module _}</th>
            <th>{_ Template _}</th>
            <th>{_ Line _}</th>
            <th>{_ Included template _}</th>
            <th>{% trans "Type of {include}" include="include" %}</th>
        </tr>
    </thead>
    <tbody>
        {% for tpl, references in references_by_file %}
            {% for info in references %}
            <tr>
                {% with tpl|development_filename|split:"/priv/templates/" as ref %}
                    <td>
                        {{ ref[1]|escape_check }}
                    </td>
                    <td>
                        {{ ref[2]|escape_check }}
                        {% button text=_"View" class="btn btn-primary btn-xs" postback={template_view template=tpl} delegate=`mod_development` %}
                        {% include "_development_editor_button.tpl" editor_file=tpl editor_line=info.line editor_column=info.column editor_label=_"Edit" editor_compact editor_title=_"Edit template" %}
                    </td>
                {% endwith %}
                <td>
                    {{ info.line }}
                </td>
                <td>
                    {% if info.has_matches %}
                        {% include "_development_dispatch_template_option.tpl" template_option=info.option %}
                    {% else %}
                        {{ info.template|escape }} <span class="label label-danger">{_ Missing template _}</span>
                    {% endif %}
                </td>
                <td>
                    {% if info.method == `optional` %}
                        optional
                    {% elseif info.method == `all` %}
                        all
                    {% endif %}
                    {% if info.is_catinclude %}
                        catinclude
                    {% else %}
                        include
                    {% endif %}
                </td>
            </tr>
            {% endfor %}
        {% empty %}
            <tr>
                <td colspan="5">
                    <span class="text-muted">
                        {_ No references found. _}
                    </span>
                </td>
            </tr>
        {% endfor %}
    </tbody>
</table>
