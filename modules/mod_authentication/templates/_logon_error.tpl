<div id="logon_error_pw">
	<h2>{_ You entered an unknown username or password.  Please try again. _}</h2>

	<p>
		{_ You might have made a typo in your username or password.  Please note that both are case sensitive and check that your caps lock key is off. _}
	</p>

	<h3>{_ I forgot my username or password _}</h3>

	<p>{_ When you forgot your username or pasword then you can ask us to _} <a id="{{ #logon_reminder }}" href="{% url logon_reminder %}">{_ e-mail a temporary password _}</a>.  {_ The e-mail will contain instructions how to reset your password. _}</p>

	{% wire id=#logon_reminder action={set_class target="logon_outer" class="logon_reminder"} %}
</div>

<div id="logon_error_reminder">
	<h2>{_ You entered an unknown username or e-mail address.  Please try again. _}</h2>
	
	<p>{_ We can only send you an e-mail when we have the e-mail address of your account. _}</p>
	<p>{_ To find your account	you need to enter either your username or the e-mail address you gave us. _}</p>
</div>

<div id="logon_error_password_tooshort">
	<h2>{_ Your new password is too short. _}</h2>

	<p>{_ Passwords should have at least six characters. _}<p>
	<p>{_ Use some non alphabetical characters or digits to make it harder to guess. _}</p>
</div>

<div id="logon_error_password_unequal">
	<h2>{_ The two passwords should be equal. Please retype them. _}</h2>

	<p>{_ Passwords should have at least six characters. _}<p>
	<p>{_ Use some non alphabetical characters or digits to make it harder to guess. _}</p>
</div>

<div id="logon_error_verification">
	<h2>{_ Sorry, could not send the verification message. _}</h2>
	
	<p>{_ We don’t seem to have any valid e-mail address or other electronic communication address of you. _}</p>
</div>
