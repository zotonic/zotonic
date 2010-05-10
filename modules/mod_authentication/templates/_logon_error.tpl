<div id="logon_error_pw">
	<h2>You entered an unknown username or password. Please try again.</h2>

	<p>
		You might have made a typo in your username or password.  Please note that both are case sensitive and check that your caps lock key is off.
	</p>

	<h3>I forgot my username or password</h3>

	<p>When you forgot your username or pasword then you can ask us to <a id="{{ #logon_reminder }}" href="{% url logon_reminder %}">e-mail a temporary password</a>.  The e-mail will contain instructions how to reset your password.</p>

	{% wire id=#logon_reminder action={set_class target="logon_outer" class="logon_reminder"} %}
</div>

<div id="logon_error_reminder">
	<h2>You entered an unknown username or e-mail address.  Please try again.</h2>
	
	<p>We can only send you an e-mail when we have the e-mail address of your account.</p>
	<p>To find your account	you need to enter either your username or the e-mail address you gave us.</p>
</div>
