{% if id %}
{% with m.rsc[id].o.subject as tags %}
{% if tags %}
<p class="keywords">
    {_ Keywords _}: 
    {% for id in tags %}
    <a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a>{% if not forloop.last %},{% endif %}
    {% endfor %}
</p>
{% endif %}
{% endwith %}
{% endif %}

{% cache 3600 cat='article' %}

<h2>{_ Archive _}</h2>
<ul class="simple-list">
    {% for year, months in m.search[{archive_year_month cat='article'}] %}
    <li><a class="caption" href="{% url archives_y year=year %}">{{ year }}</a>
    <ul>
        {% for row in months %}
        <li><a href="{% url archives_m year=year month=row.month %}">{{ row.month_as_date|date:"F" }}</a> ({{ row.count }}){% if not forloop.last %},{% else %}.{% endif %}</li>
        {% endfor %}
    </ul>
    </li>
    {% endfor %}
</ul>
{% endcache %}
