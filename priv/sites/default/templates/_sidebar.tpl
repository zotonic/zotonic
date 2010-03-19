{# Cache the sidebar, depending on the stuff in the 'article' category #}
{% cache 3600 cat='article' %}

{% include "_article_keywords.tpl" %}

<h2>Archive</h2>
<ul class="simple-list">
	{% for year, months in m.search[{archive_year_month cat='article'}] %}
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

<h2>Keywords</h2>
<ul class="simple-list">
    {% for id, count in m.search[{keyword_cloud cat='article'}] %}
    	<li><a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a> ({{ count }})</li>
    {% endfor %}
</ul>

{% endcache %}
