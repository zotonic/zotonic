{% extends "admin_base.tpl" %}

{% block title %}Webmachine Trace List{% endblock %}

{% block content %}

	<div id="content" class="zp-85">
		<div class="block clearfix">

		<h2>Webmachine Trace List</h2>

			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-90">Filename</span>
					<span class="zp-10 last">Options</span>
				</li>

			{% for filename in files %}
				<li>
					<a href="{% url wmtrace star=filename %}" class="clearfix">
						<span class="zp-90">{{ filename|escape }}</span>
						<span class="zp-10">
							{% button text="view" action={redirect dispatch="wmtrace" star=filename} %}
						</span>
					</a>
				</li>
			{% empty %}
				<li>
					No webmachine traces found.
				</li>
			{% endfor %}
			</ul>

		</div>
	</div>

{% endblock %}
