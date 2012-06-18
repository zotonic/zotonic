{% extends "base.tpl" %}

{% block title %}{_ Search _}{% if q.qs %}: {{ q.qs|escape }}{% endif %}{% endblock %}

{% block page_class %}search{% endblock %}

{% block content %}
{% if q.qs %}
{% with m.search.paged[{query text=q.qs cat=`text` cat=`collection` cat=`document` cat=`location` cat=`mailinglist` pagelen=10 page=q.page}] as result %}

	<h1>{_ Searching for _} <b>{{ q.qs|escape }}</b>
		{% if result.total > 0 %}
			<small>{_ Page _} {{ result.page }}/{{ result.pages }}</small>
		{% endif %}
	</h1>

	<div class="row-fluid">
		<div class="span8 main">
			{% if result.total > 0 %}
				{% include "_content_list.tpl" list=result is_large %}
				{% pager result=result %}
			{% else %}
			<p id="content-pager">
				{_ Did not find any pages matching _} <b>{{ q.qs|escape }}</b>.
			</p>
			{% endif %}
			{% include "_search_form.tpl" %}
		</div>

		<div id="subnavbar" class="span4">
			{% include "_subnav.tpl" %}
		</div>
	</div>
{% endwith %}
<hr/>
{% else %}
<div class="row-fluid">
	<div class="span8 main">
		{% include "_search_form.tpl" %}
	</div>
	<div id="subnavbar" class="span4">
		{% include "_subnav.tpl" %}
	</div>
</divv>
{% endif %}

{% endblock %}
