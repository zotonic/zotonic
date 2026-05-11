<html>
{% with m.rsc[q.id].id|default:id as id %}
{% with m.survey.all_results[[id, q.sort|default:"name_surname"]] as rs %}
{% with rs[1] as headers %}
{% with rs|tail as rows %}
<head>
    <title>{{ id.title|default:id.short_title }}</title>

    <style type="text/css" nonce="{{ m.req.csp_nonce }}">
    body {
        font-family: Arial, Helvetica, Sans-Serif;
        font-size: 12px;
    }
    table {
        font-size: 12px;
        border-spacing: 1px;
        border-collapse: collapse;
    }
    th {
        text-align: center;
        font-weight: 400;
        border-bottom: 1px solid #ccc;
        border-right: 1px solid #ccc;
        padding: 4px;
    }
    td {
        text-align: left;
        border-bottom: 1px solid #ccc;
        border-right: 1px solid #ccc;
        padding: 2px 4px;
    }
    .totals td {
        border-top: 1px solid #ccc;
        font-weight: 400;
    }
    </style>
</head>

<body>
    <h1>{{ id.title|default:id.short_title }}</h1>

    <p>{_ Results until _} <strong>{{ now|date:"Y-m-d H:i" }}</strong> ({{ rows|length }})</p>

    <table>
        <thead>
            {% with m.survey.captions[id] as captions %}
            <tr class="header">
                <th>&nbsp;</th>
                {% for name in headers %}
                    <th>{{ captions[name]|default:name|capfirst }}</th>
                {% endfor %}
            </tr>
            {% endwith %}
        </thead>

        <tbody>
            {% for ans_id,ans in rows %}
            <tr id="survey-result-{{ ans_id }}">
                <td style="text-align: right">{{ forloop.counter }}.</td>
                {% for value in ans %}
                    <td {% if value|match:"^[0-9]+(\\.[0-9]*)?$" %}style="text-align: right"{% endif %}>{{ value|escape|linebreaksbr }}</td>

                {% endfor %}
            </tr>
            {% endfor %}
        </tbody>

        {% if m.survey.totals[id] as totals %}
            <tfoot>
                <tr class="totals">
                    <td style="text-align: right">{_ Totals _}&nbsp;</td>
                    {% for name in headers %}
                        {% if totals[name]|is_number %}
                            <td style="text-align: right; border-top: 2px solid #999;">
                                {{ totals[name] }}
                            </td>
                        {% else %}
                            <td></td>
                        {% endif %}
                    {% endfor %}
                </tr>
            </tfoot>
        {% endif %}
    </table>
</body>
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
</html>
