{% extends "logon_base.tpl" %}

{% block logon_area %}

<style type="text/css">

/* Design CSS */

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
	text-align: left;
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

.logon_error div#logon_box {
	width: 600px;
}

div#logon_error {
	margin: 18px 18px 18px 0px;
	padding-right: 18px;
	border-right: 1px solid #ccc;
	width: 260px; 
	float: left; 
	text-align: left;	
}


div#logon_error h2 {
	color: #d80000;
}

div#logon_form_box,
div#logon_password_reset,
div#logon_reminder,
div#logon_verification_pending,
div#logon_verification_sent {
	width: 300px;
}

div#logon_button {
	text-align: center;
}

div#logon_button button {
	float: none;
	display: inline;
	font-weight: bold;
	padding: 4px 10px;
}

h1.logon_header {
	margin: 18px 0px;
}

div.rememberme {
	margin-top: 9px;
}

div.rememberme label {
	float: none;
	display: inline;
}

p.logon_link {
	margin: 0;
}

#logon_methods img {
	float: none;
}


/* Overlay label and input element */
/* This should be part of the z.inputoverlay.js module. */

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


/* Display logic, hide/show forms depending on page state */

div#logon_error {
	display: none;
}

.logon_error div#logon_error {
	display: block;
}

div#logon_pw,
div#logon_form_box,
div#logon_reminder,
div#logon_reminder_sent,
div#logon_password_reset,
div#logon_verification_pending,
div#logon_verification_sent {
	display: none;
}

.logon_pw div#logon_pw,
.logon_pw div#logon_form_box,
.logon_reminder div#logon_reminder,
.logon_reminder_sent div#logon_reminder_sent,
.logon_password_reset div#logon_password_reset,
.logon_verification_pending div#logon_verification_pending,
.logon_verification_sent div#logon_verification_sent {
	display: block;
}

.logon_reminder_sent form#logon_reminder_form {
	display: none;
}

a#logon_pw_link {
	display: none;
}

a#logon_reminder_link {
	display: inline;
}

.logon_reminder a#logon_pw_link,
.logon_verification_pending a#logon_pw_link,
.logon_verification_sent a#logon_pw_link {
	display: inline;
}

.logon_reminder a#logon_reminder_link {
	display: none;
}

.logon_password_reset a#logon_pw_link {
	display: inline;
}

.logon_password_reset a#logon_reminder_link,
.logon_verification_pending a#logon_reminder_link,
.logon_verification_sent a#logon_reminder_link {
	display: none;
}

div#logon_error_pw,
div#logon_error_reminder,
div#logon_error_password_tooshort,
div#logon_error_password_unequal,
div#logon_error_verification {
	display: none;
}

.logon_pw div#logon_error_pw,
.logon_reminder div#logon_error_reminder,
.logon_error_password_tooshort div#logon_error_password_tooshort,
.logon_error_password_unequal div#logon_error_password_unequal,
.logon_error_verification div#logon_error_verification {
	display: block;
}

</style>


<div id="logon_outer" class="{% if is_password_reset %}logon_password_reset{% else %}logon_pw{% endif %}">
	{% if logon_reason %}
	<p id="logon_reason">{{ logon_reason }}</p>
	{% endif %}

	<div id="logon_box">
		
		<div id="logon_error">
			{% include "_logon_error.tpl" %}
		</div>

		<div id="logon_dialog" style="float: left">
			{% include "_logon_form_box.tpl" %}
			{% include "_logon_password_reminder.tpl" %}
			{% include "_logon_verification.tpl" %}
			{% if is_password_reset %}
				{% include "_logon_password_reset.tpl" %}
			{% endif %}
		</div>
		
		<div style="clear: both"></div>
	</div>

	{% wire action={script script="$('#username').focus();"} %}
	
	<ul id="logon_methods">
		{% all include "_logon_extra.tpl" %}
	</ul>
	
	<p class="logon_link"><a id="logon_reminder_link" href="">I forgot my username or password.</a></p>
	{% wire id="logon_reminder_link" action={set_class target="logon_outer" class="logon_reminder"} %}

	<p class="logon_link"><a id="logon_pw_link" href="">Please show me the log on form.</a></p>
	{% wire id="logon_pw_link" action={set_class target="logon_outer" class="logon_pw"} %}

	{% all include "_logon_link.tpl" %}

</div>

{# Use a real post for all forms on this page, and not AJAX or Websockets.
   This will enforce all cookies to be set correctly. #}
{% wire action={script script="z_only_post_forms = true;"} %}

{% endblock %}
