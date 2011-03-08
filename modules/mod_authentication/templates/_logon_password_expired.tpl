<div id="logon_password_expired">
	<form id="logon_password_expired_form" method="post" action="postback">
		<h1 class="logon_header">{_ You need to change your password _}</h1>

		<p>{_ Below you can enter a new password for your account _}</p>

		<input type="hidden" id="logon_password_expired_secret" name="secret" value="" />

		<div id="logon_password_reset1">
			<p class="do_inputoverlay">
				<span>{_ New password _}</span>
				<input type="password" id="password_reset1" name="password_reset1" value="" autocomplete="off" />
			</p>
		</div>

		<div id="logon_password_reset2">
			<p class="do_inputoverlay">
				<span>{_ Repeat password _}</span>
				<input type="password" id="password_reset2" name="password_reset2" value="" autocomplete="off" />
			</p>
		</div>

		<div id="logon_password_reset_rememberme" class="rememberme">
			<input type="checkbox" id="rememberme" name="rememberme" value="1" />
			<label for="rememberme">{_ Stay logged on unless I log off. _}</label>
		</div>

		<div class="clearfix"></div>

		<div id="logon_reset_button">
			<button type="submit">{_ Change password and Log on _}</button>
		</div>

	</form>
</div>
