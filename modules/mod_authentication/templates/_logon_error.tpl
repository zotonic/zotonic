
{% if reason == "pw" %}
<p>
    {_ Either the email or the password you entered is incorrect. Please check your entry and try again. _}
</p>

<p><a href="{% url logon_reminder %}" id="logon_error_link_reminder">{_ Need help signing in? _}</a></p>

{% elseif reason == "reminder" %}

<h4>{_ You entered an unknown username or e-mail address.  Please try again. _}</h4>
	
<p>{_ We can only send you an e-mail when we have the e-mail address of your account. _}</p>
<p>{_ To find your account	you need to enter either your username or the e-mail address you gave us. _}</p>

{% elseif reason == "tooshort" %}

<h4>{_ Your new password is too short. _}</h4>

<p>{_ Passwords should have at least six characters. _}<p>
<p>{_ Use some non alphabetical characters or digits to make it harder to guess. _}</p>

{% elseif reason == "unequal" %}

<h4>{_ The two passwords should be equal. Please retype them. _}</h4>

<p>{_ Passwords should have at least six characters. _}<p>
<p>{_ Use some non alphabetical characters or digits to make it harder to guess. _}</p>


{% endif %}


{% javascript %}
$("#logon_form form").unmask();
$("#logon_form #username").focus();
{% endjavascript %}
