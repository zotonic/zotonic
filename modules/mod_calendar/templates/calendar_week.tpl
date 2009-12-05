{% extends "page.tpl" %}

{% block title %}Week calendar of {{date|date:"Y, F d"}}{% endblock %}

{% block content %}
	<section id="content-wrapper" class="clearfix">
		<article id="content" class="zp-100">
			<div class="padding">

				<p>
					<a name="calview"></a>
					
					<a href="{% url calendar date=date|sub_week|date:'Ymd' %}#calview">&nbsp;&lt;&nbsp;</a>
					<a href="{% url calendar %}">today</a>
					<a href="{% url calendar date=date|add_week|date:'Ymd' %}#calview">&nbsp;&gt;&nbsp;</a>
				</p>
			
				{% calview date=date weekstart=weekstart timestart=timestart %}
				
			</div>
		</article>
	</section>
{% endblock %}
