{% extends "base.tpl" %}

{% block title %}{_ Sites _}{% endblock %}
{% block page_class %}page{% endblock %}

{% block content %}
<article id="content" class="zp-67">
	<div class="padding">
		<h1>{_ Sites on this Zotonic server _}</h1>
	
		<table id="sites">
			{% include "_sites.tpl" %}
		</table>
	</div>
</article>
{% endblock %}



{% block sidebar %}
<aside id="sidebar" class="zp-33">
	{% if not has_user %}
	<div id="logon_form_box">
		<h2>{_ Log on to manage this server _}</h2>
		<p>{_ You will find the password in the <tt>priv/config</tt> file. _}</p>
		<form id="logon_form" action="postback">
			<p class="error">The password does not match.  Please retry.</p>
			<div id="logon_password">
				<p>
					<span>{_ Password _}</span>
					<input type="password" id="password" name="password" value="" autocapitalize="off" autocomplete="off" />
				</p>
			</div>

			{#
			<div id="logon_rememberme" class="rememberme">
				<label for="{{ #rememberme }}" class="wide"><input type="checkbox" id="{{ #rememberme }}" name="rememberme" value="1" /> {_ Remember me for two weeks. _}</label>
			</div>
			
			<div class="clearfix"></div>
			#}

			<div id="logon_button">
				<button>{_ Log On _}</button>
			</div>
		</form>
	</div>
	{% else %}
	<div>
		{% all include "_z_system_button.tpl" %}
                {% all include "_z_trace_button.tpl" %}
	</div>
	<div style="clear:left" id="notices"></div>
	{% endif %}
</aside>
{% endblock %}


