{% extends "base.tpl" %}

{% block title %}Log on{% endblock %}

{% block content %}

{% if logon_reason %}
<p id="logon_reason">{{ logon_reason }}</p>
{% else %}
	{% if page %}
	<p id="logon_reason">You need to log on to view the page at <a href="{{page|escape}}">{{ page|truncate:20|escape }}</a>.
	{% endif %}
{% endif %}

<div id="logon">
	<div id="logon_dialog" class="clearfix">
		<form id="logon_form" method="post" action="postback">
			<h1 id="logon_header">Log on using your <span>{{ m.config.site.title.value|default:"Zotonic" }} ID</span></h1>
			
			<p class="error" style="display: none" id="error">Unknown username or password.</p>

			<input type="hidden" name="page" value="{{ page|escape }}" />
			<input type="hidden" name="handler" value="username" />

			<div id="logon_username">
				<label for="username">Username</label>
				<input type="text" id="username" name="username" value="" />
				{% validate id="username" type={presence} %}
			</div>

			<div id="logon_username">
				<label for="password">Password</label>
				<input type="password" id="password" name="password" value="" />
				{% validate id="password" type={presence} %}
			</div>
		
			<div id="logon_rememberme">
				<input type="checkbox" id="rememberme" name="rememberme" value="1" />
				<label for="rememberme">Remember me</label>
			</div>
		
			<div class="clearfix"></div>
		
			<div id="logon_button">
				<button>Log On</button>
			</div>
		</form>
	</div>

	<!-- 
	<ul id="logon_methods">
		<li><a href="#">Log on using OpenID</a></li>
		<li><a href="#">Log on using Facebook</a></li>
	</ul>
	-->
</div>

{% endblock %}

{% block sidebar %}{% endblock %}
{% block navigation %}{% endblock %}
