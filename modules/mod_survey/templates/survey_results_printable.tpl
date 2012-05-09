{% with q.id|default:id as id %}
{% with m.survey.all_results[id] as r %}
{% with r|first as columns %}
{% with r|tail as results %}

{% with m.survey.captions[id] as captions %}

<h1>{{ m.rsc[id].title }}</h1>
<p><strong>{_ All survey entries up until _} {{ now|date:"Y-m-d H:i" }}</strong></p>

<table width="100%">
    <tr>
        <th>&nbsp;</th>
        {% for name in columns|tail|tail %}
        <th align="left">{{ captions[name] }}</th>
        {% endfor %}
    </tr>

    {% for r in results %}
    <tr id="survey-result-{{ r[1] }}-{{ r[2] }}">
        <td align="right">{{ forloop.counter }}.&nbsp;&nbsp;</td>
        {% for value in r|tail|tail %}
        <td>{{ value }}</td>
        {% endfor %}
    </tr>
    {% endfor %}
</table>

{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
