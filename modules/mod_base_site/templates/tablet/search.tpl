{% extends "base.tpl" %}

{% block title %}{_ Search _}{% if q.qs %}: {{ q.qs|escape }}{% endif %}{% endblock %}

{% block page_class %}search{% endblock %}

{% block content %}
{% if q.qs or q.qcat %}
{% with result|default_if_undefined:m.search.paged[{query 
		text=q.qs 
		cat=m.rsc[q.qcat].name|default:[`text`,`collection`,`document`,`location`,`mailinglist`]
		pagelen=10 page=q.page}] as result 
%}
	<h1>{_ Searching for _} 
		{% if q.qs %}<b class="qs">{{ q.qs|escape }}</b>{% endif %}
		{% if q.qcat %}<span class="qcat">{{ m.rsc[q.qcat].title }}</span>{% endif %}
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
				{_ Did not find any pages matching _} <b class="qs">{{ q.qs|escape }}</b>.
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
</div>
{% endif %}

{% endblock %}
