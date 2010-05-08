{% extends "base.tpl" %}

{% block title %}Log on{% endblock %}

{% block content_area %}

<style>
div#logon_outer {
	text-align: center;
	margin: 100px auto 20px;
}

p#logon_reason {
	margin-bottom: 30px;
}

ul#logon_methods {
	margin: 30px;
}

div#logon_box {
	width: 300px;
	margin: 0 auto;
	padding: 10px 30px 10px;
	border: 1px solid #aaa;

	border-radius: 8px;
	-webkit-border-radius: 8px;
	-moz-border-radius: 8px;

	box-shadow: 0 0 6px #999;
	-webkit-box-shadow: 0 0 6px #999;
	-moz-box-shadow: 0 0 6px #999;

	-ms-filter: "progid:DXImageTransform.Microsoft.Shadow(Strength=3, Direction=135, Color='#999999')";
	filter: progid:DXImageTransform.Microsoft.Shadow(Strength=3, Direction=135, Color='#999999');
}

div#logon_button button {
	float: none;
	display: inline;
	font-weight: bold;
	padding: 4px 10px;
}

div#logon_rememberme {
	margin-top: 9px;
}

p.do_inputoverlay {
	margin: 0px;
	padding: 0px;
	position: relative;
	height: 40px;
	font-size: 18px;
}

p.do_inputoverlay input {
	position: absolute;
	left: 0px;
	background: none !important;
	font-size: 18px;
}

p.do_inputoverlay span {
	position: absolute;
	left: 8px;
	top: 5px;
	color: #aaa;
}

p.do_inputoverlay span.focus {
	color: #d8d8d8;
}

p.do_inputoverlay span.hidden {
	display: none;
}

h1#logon_header {
	margin: 18px 0px;
}

#logon_rememberme label {
	float: none;
	display: inline;
}

</style>


<div id="logon_outer">
	{% if logon_reason %}
	<p id="logon_reason">{{ logon_reason }}</p>
	{% endif %}

	<div id="logon_box">
		<div id="logon_dialog" class="clearfix">
			<form id="logon_form" method="post" action="postback">
				<h1 id="logon_header">Log on using your <span>{{ m.config.site.title.value|default:"Zotonic" }} ID</span></h1>
			
				<p class="error" style="display: none" id="error">Unknown username or password.</p>

				<input type="hidden" name="page" value="{{ page|escape }}" />
				<input type="hidden" name="handler" value="username" />

				<div id="logon_username">
					<p class="do_inputoverlay">
						<span>Username</span>
						<input type="text" id="username" name="username" value="" autocapitalize="off" autocomplete="on" />
					</p>
				</div>

				<div id="logon_password">
					<p class="do_inputoverlay">
						<span>Password</span>
						<input type="password" id="password" name="password" value="" autocomplete="on" />
					</p>
				</div>
		
				<div id="logon_rememberme">
					<input type="checkbox" id="rememberme" name="rememberme" value="1" />
					<label for="rememberme">Remember me for two weeks</label>
				</div>
		
				<div class="clearfix"></div>
		
				<div id="logon_button">
					<button>Log On</button>
				</div>
			</form>
		</div>
	</div>

	{% wire action={script script="$('#username').focus();"} %}
	
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
