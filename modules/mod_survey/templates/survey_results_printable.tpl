<html>
{% with m.rsc[q.id].id|default:id as id %}
{% with m.survey.all_results[[id, q.sort|default:"name_surname"]] as r %}
{% with r|first as columns %}
{% with r|tail as results %}
<head>
    <title>{{ id.title }}</title>

    <style type="text/css">
    body {
        font-family: Helvetica, Arial, Sans-Serif;
        font-size: 0.8em;
    }
    table {
        border-collapse: collapse;
    }
    th {
        text-align: left;
        font-weight: 400;
        border-bottom: 1px solid #ccc;
        padding: 0.1em;
    }
    td {
        text-align: right;
        padding: 0.1em;
    }
    .totals td {
        border-top: 1px solid #ccc;
        font-weight: 400;
    }
    </style>
</head>

<body>
    <h1>{{ m.rsc[id].title }}</h1>

    <p>{_ All survey entries up until _} <strong>{{ now|date:"Y-m-d H:i" }}</strong> ({{ results|length }} {_ Results _})</p>

    <table width="100%">
        {% with m.survey.captions[id] as captions %}
        <tr class="header">
            <th>&nbsp;</th>
            {% for name in columns|tail|tail|tail %}
                <th>{{ captions[name]|default:name|capfirst }}</th>
            {% endfor %}
        </tr>
        {% endwith %}

        {% for r in results %}
        <tr id="survey-result-{{ r[1] }}-{{ r[2] }}">
            <td>{{ forloop.counter }}.&nbsp;&nbsp;</td>
            {% for value in r|tail|tail|tail %}
            <td>{{ value }}</td>
            {% endfor %}
        </tr>
        {% endfor %}

        {% with m.survey.totals[id] as totals %}
            {% if totals %}
                <tr class="totals">
                    <td align="right">{_ Totals _}&nbsp;</td>
                    {% for name in columns|tail|tail|tail %}
                        <td align="left">{{ totals[name]|default:"&nbsp;" }}</td>
                    {% endfor %}
                </tr>
            {% endif %}
        {% endwith %}
    </table>
</body>
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
</html>
