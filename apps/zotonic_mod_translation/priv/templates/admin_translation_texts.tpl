<html {% include "_language_attrs.tpl" class=false language=z_language %} class="zotonic-admin environment-{{ m.site.environment }}">
    <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta name="description" content="">
        <meta name="robots" content="noindex,nofollow">

        <title>{_ Translations _}: {{ id.title }}</title>

        <link rel="icon" href="/favicon.ico" type="image/x-icon" />
        <link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" />
        <link rel="manifest" href="{% url manifest_json %}" />

        {% lib
            "css/admin-bootstrap3.css"
            minify
        %}

        {% lib
            "css/z.modal.css"
            "css/z.icons.css"
            "css/z.bridge.css"
            "css/logon.css"
            "css/jquery.loadmask.css"
            "css/zotonic-admin.css"
            "css/prism.css"
            minify
        %}

        {% all include "_html_head_admin.tpl" no_prism %}

        {% block head_extra %}
        {% endblock %}
    </head>

    <body id="body" class="{% block bodyclass %}{% endblock %}"{% block bodyattr %}{% endblock %} data-cotonic-pathname-search="{% cotonic_pathname_search %}">
        <h1>
            {{ id.title|default:id.short_title|default:_"<em>Untitled</em>" }}

            {% if q.close %}
                <a id="link-close" href="{% url admin_edit_rsc id=id %}" class="btn btn-primary">{_ Close _}</a>
                {% javascript %}
                   document.getElementById('link-close').addEventListener("click", () => {
                        window.close();
                    });
                {% endjavascript %}
            {% else %}
                <a id="link-edit" href="{% url admin_edit_rsc id=id %}" class="btn btn-primary">{_ Edit _}</a>
            {% endif %}

        </h1>

        <table class="table table-striped">
            <thead style="position: sticky; top: 0; background-color: white">
                <tr>
                    <th width="5%">{_ Field _}</th>
                    {% for lang in id.language|default:[ z_language ] %}
                        <th>{{ lang }}</th>
                    {% endfor %}
                </tr>
            </thead>
            <tbody>
                {% for p in [
                        'title',
                        'short_title',
                        'summary',
                        'body',
                        'body_extra'
                    ]
                %}
                    <tr>
                        <th>{{ p }}</th>
                        {% for lang in id.language|default:[ z_language ] %}
                            <td>{{ m.rsc[id][p] with z_language=lang }}</td>
                        {% endfor %}
                    </tr>
                {% endfor %}

                <tr style="border-bottom: 2px solid black">
                    <th></th>
                    {% for lang in id.language|default:[ z_language ] %}
                        <td></td>
                    {% endfor %}
                </tr>

                {% for b in id.blocks %}
                    {% if b.type == 'survey_page_break' %}
                        <tr style="border-bottom: 2px solid black;">
                            <th><small class="text-muted">{_ page break _}</small></th>
                            {% for lang in id.language|default:[ z_language ] %}
                                <td></td>
                            {% endfor %}
                        </tr>
                    {% elseif b.type|match:"survey_.*" %}
                        <tr>
                            <th rowspan="3">{{ b.name }}<br><small class="text-muted">{{ b.type }}</small></th>
                            {% for lang in id.language|default:[ z_language ] %}
                                <td>{{ b.prompt with z_language=lang }}</td>
                            {% endfor %}
                        </tr>
                        <tr>
                            {% for lang in id.language|default:[ z_language ] %}
                                <td>{{ b.explanation with z_language=lang }}</td>
                            {% endfor %}
                        </tr>
                        <tr>
                            {% for lang in id.language|default:[ z_language ] %}
                                <td>
                                    {% if b.type == 'survey_thurstone' %}
                                        <ol>
                                            {% for ans in b.answers %}
                                                <li>{{ ans.option with z_language=lang }}</li>
                                            {% endfor %}
                                        </ol>
                                    {% elseif b.type == 'survey_yesno' %}
                                        <ol>
                                            <li>{{ b.yes|default:_"Yes" with z_language=lang }}</li>
                                            <li>{{ b.no|default:_"No" with z_language=lang }}</li>
                                        </ol>
                                    {% endif %}
                                </td>
                            {% endfor %}
                        </tr>
                    {% else %}
                        <tr>
                            <th>{{ b.name }}<br><small class="text-muted">{{ b.type }}</small></th>
                            {% for lang in id.language|default:[ z_language ] %}
                                {% if b.type == 'text' %}
                                    <td>{{ b.body with z_language=lang }}</td>
                                {% elseif b.type == 'header' %}
                                    <td>{{ b.header with z_language=lang }}</td>
                                {% elseif b.type == 'page' %}
                                    <td>{{ b.rsc_id.title with z_language=lang }}</td>
                                {% endif %}
                            {% endfor %}
                        </tr>
                    {% endif %}
                {% endfor %}
            </tbody>
        </table>

        {% include "_bridge_warning.tpl" %}

        {% include "_admin_js_include.tpl" %}
        {% block js_extra %}{% endblock %}

        {% block html_body_admin %}{% all include "_html_body_admin.tpl" %}{% endblock %}

        {% script %}
    </body>
</html>
