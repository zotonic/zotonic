<div id="logon_password_reset">
	{% if username %}
	<form id="logon_password_reset_form" method="post" action="postback">
		<h1 class="logon_header">{_ Reset your password _}</h1>

		<p>{_ Below you can enter a new password for your account _} <strong>{{ username|escape }}</strong>.</p>

		<input type="hidden" name="secret" value="{{ secret|escape }}" />

		<div id="logon_password_reset1">
			<p class="do_inputoverlay">
				<span>{_ New password _}</span>
				<input type="password" id="password_reset1" name="password_reset1" value="" autocomplete="off" />
				{% validate id="password_reset1" type={presence} %}
			</p>
		</div>

		<div id="logon_password_reset2">
			<p class="do_inputoverlay">
				<span>{_ Repeat password _}</span>
				<input type="password" id="password_reset2" name="password_reset2" value="" autocomplete="off" />
				{% validate id="password_reset2" type={presence} type={confirmation match="password_reset1"} %}
			</p>
		</div>

		<div id="logon_password_reset_rememberme" class="rememberme">
			<input type="checkbox" id="rememberme" name="rememberme" value="1" />
			<label for="rememberme">{_ Stay logged on unless I log off. _}</label>
		</div>

		<div class="clearfix"></div>

		<div id="logon_reset_button">
			<button type="submit">{_ Reset Password and Log On _}</button>
		</div>

		<p class="clear">
			{_ Your password will be reset and you will be logged on as _} <strong>{{ username|escape }}</strong>.
		</p>

	</form>
	{% else %}
	<h1 class="logon_header">{_ Sorry, your password reset code is unknown or expired _}</h1>
	
	<p>{_ For security reasons password reset codes are only kept for a limited amount of time and can only be used once. _}</p>
	<p>{_ You can _}<a id="{{ #new_pw }}" href="">{_ request a new password reset code _}</a>.</p>
	
	{% wire id=#new_pw action={set_class target="logon_outer" class="logon_reminder"} %}
	
	{% endif %}
</div>

