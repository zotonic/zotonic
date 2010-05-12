{# 
 When there is an authenticated user, do not show the button etc. 
 As Facebook automatically logs on whenever the log in button is shown.
 Only allow a Facebook log on when there is no authenticated user.
#}

<li id="logon_facebook">
	<a href="{% url facebook_authorize %}"><img src="/lib/images/fb-login-button.png" width="154" height="22" alt="Facebook login button" /></a>
</li>
