{% extends "page.tpl" %}

{% block title %}Week calendar of {{date|date:"Y, F d"}}{% endblock %}

{% block content %}
	<section id="content-wrapper" class="clearfix">
		<article id="content" class="zp-100">
			<div class="padding">
			
				{% calview date=date weekstart=weekstart timestart=timestart %}
				
			</div>
		</article>
	</section>
{% endblock %}
