<div id="logon_reminder">
	<form id="logon_reminder_form" method="post" action="postback">
		<h1 class="logon_header">{_ Forgot your password? _}</h1>

		<p>{_ Enter your e-mail address or username below and we will send you an e-mail with password reset instructions. _}</p>

		<div id="logon_reminder_email">
			<p class="do_inputoverlay">
				<span>{_ E-mail address or username _}</span>
				<input type="text" id="reminder_address" name="reminder_address" value="{% if m.acl.user %}{{ m.identity[m.acl.user].username|escape }}{% endif %}" autocapitalize="off" autocomplete="on" />
			</p>
		</div>

		<div class="clearfix"></div>

		<div id="logon_button">
			<button type="submit">{_ Send me instructions _}</button>
		</div>
	</form>

	<div id="logon_reminder_sent">
		<h1 class="logon_header">We sent you an e-mail</h1>

		<p>{_ In the e-mail you will find instructions on how to reset the password of your account. _}</p>
		<p>{_ When you don’t receive the e-mail within a few minutes then be sure to check your spam filter and spam folders. _}</p>
	</div>
</div>

