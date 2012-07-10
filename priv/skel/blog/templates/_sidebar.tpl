{# Cache the sidebar, depending on the stuff in the 'article' category #}
{% cache 3600 cat='article' %}

	{% if m.category.tweet %}

		{% with m.search[{latest cat='tweet' pagelen=4}] as r %}
			{% if r.result %}
				<h2>Tweets</h2>
				<ul class="shouts-list">
					{% for tw in r.result %}
						<li>
							<p>
								<img width="28" height="28" src="{{ m.rsc[tw].tweet['user'][2]['profile_image_url'] }}" />
								<span class="tweet-body">{{ m.rsc[tw].body|twitter }}</span>
								<span class="tweet-date-time">from {{ m.rsc[tw].tweet["source"] }} by {{ m.rsc[tw].tweet['user'][2]['screen_name'] }}</span>
							</p>
						</li>
					{% endfor %}
				</ul>
			{% endif %}
		{% endwith %}

	{% endif %}

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

	<h2>{_ Keywords _}</h2>
	<ul class="inline-list clearfix">
		{% for id, count in m.search[{keyword_cloud cat='article'}] %}
		<li><a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a> ({{ count }}){% if not forloop.last %},{% else %}.{% endif %}</li>
		{% endfor %}
	</ul>

{% endcache %}

{% include "_edit_button.tpl" %}
