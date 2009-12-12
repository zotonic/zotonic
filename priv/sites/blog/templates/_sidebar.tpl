
<h3>Archive</h3>
<ul class="archive">
    {% for year, months in m.search[{archive_year_month cat='text'}] %}
    <li>
        <a href="{% url archives_y year=year %}">{{ year }}</a>
        <ul>
            {% for row in months %}
            <li><a href="{% url archives_m year=year month=row.month %}">{{ row.month_as_date|date:"F" }}</a> ({{ row.count }})</li>
            {% endfor %}
        </ul>
    </li>
    {% endfor %}
</ul>     

<h3>Keywords</h3>
<ul>
    {% for id, count in m.search[{keyword_cloud cat='text'}] %}
    <li><a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a> ({{ count }})</li>
    {% endfor %}
</ul>
