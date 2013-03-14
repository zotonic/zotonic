{% with q.id|default:id as id %}
{% with m.survey.all_results[[id, q.sort|default:"name_surname"]] as r %}
{% with r|first as columns %}
{% with r|tail as results %}


<h1>{{ m.rsc[id].title }}</h1>
<p><strong>{_ All survey entries up until _} {{ now|date:"Y-m-d H:i" }}</strong> ({{ results|length }} {_ Results _})</p>

<table width="100%">
    {% with m.survey.captions[id] as captions %}
    <tr>
        <th>&nbsp;</th>
        {% for name in columns|tail|tail|tail %}
        <th align="left">{{ captions[name] }}</th>
        {% endfor %}
    </tr>
    {% endwith %}

    {% for r in results %}
    <tr id="survey-result-{{ r[1] }}-{{ r[2] }}">
        <td align="right">{{ forloop.counter }}.&nbsp;&nbsp;</td>
        {% for value in r|tail|tail|tail %}
        <td>{{ value }}</td>
        {% endfor %}
    </tr>
    {% endfor %}

    {% with m.survey.totals[id] as totals %}
        {% if totals %}
            <tr>
                <th align="right">{_ Totals _}&nbsp;</th>
                {% for name in columns|tail|tail|tail %}
                    <th align="left">{{ totals[name]|default:"&nbsp;" }}</th>
                {% endfor %}
            </tr>
        {% endif %}
    {% endwith %}

</table>

{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
