{% extends "page.tpl" %}

{% block title %}Calendar of {{date|date:"F Y"}}{% endblock %}

{% block content %}
	<section id="content-wrapper" class="clearfix">
		<article id="content" class="zp-100">
			<div class="padding">

				<p>
					<a name="calview"></a>
					
					<a href="{% url calendar date=date|sub_month|date:'Ym' %}">&nbsp;&lt;&nbsp;</a>
					<a href="{% url calendar %}">today</a>
					<a href="{% url calendar date=date|add_month|date:'Ym' %}">&nbsp;&gt;&nbsp;</a>
				</p>
			
				{% calview date=date weekstart=weekstart timestart=timestart %}
				
			</div>
		</article>
	</section>
{% endblock %}
