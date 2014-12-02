
{% if reason == "pw" %}
<h4>{_ You entered an unknown username or password.  Please try again. _}</h4>
<p>
    {_ You might have made a typo in your username or password.  Please note that both are case sensitive and check that your caps lock key is off. _}
</p>

<strong>{_ I forgot my username or password _}</strong>

<p>{_ When you forgot your username or pasword then you can ask us to _} <a href="{% url logon_reminder %}">{_ e-mail a temporary password _}</a>.  {_ The e-mail will contain instructions how to reset your password. _}</p>

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
