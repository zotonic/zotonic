<div id="logon_password_reset">
	{% if username %}
	<form id="logon_password_reset_form" method="post" action="postback">
		<h1 class="logon_header">Reset your password</h1>

		<p>Below you can enter a new password for your account <strong>{{ username|escape }}</strong>.</p>

		<input type="hidden" name="secret" value="{{ secret|escape }}" />

		<div id="logon_password_reset1">
			<p class="do_inputoverlay">
				<span>New password</span>
				<input type="password" id="password_reset1" name="password_reset1" value="" autocomplete="off" />
			</p>
		</div>

		<div id="logon_password_reset2">
			<p class="do_inputoverlay">
				<span>Repeat Password</span>
				<input type="password" id="password_reset2" name="password_reset2" value="" autocomplete="off" />
			</p>
		</div>

		<div id="logon_password_reset_rememberme" class="rememberme">
			<input type="checkbox" id="rememberme" name="rememberme" value="1" />
			<label for="rememberme">Remember me for two weeks</label>
		</div>

		<div class="clearfix"></div>

		<div id="logon_reset_button">
			<button>Reset Password and Log On</button>
		</div>

		<p class="clear">
			Your password will be reset and you will be logged on as <strong>{{ username|escape }}</strong>.
		</p>

	</form>
	{% else %}
	<h1 class="logon_header">Sorry, your password reset code is unknown or expired</h1>
	
	<p>For security reasons password reset codes are only kept for a limited amount of time and can only be used once.</p>
	<p>You can <a id="{{ #new_pw }}" href="">request a new password reset code</a>.</p>
	
	{% wire id=#new_pw action={set_class target="logon_outer" class="logon_reminder"} %}
	
	{% endif %}
</div>

