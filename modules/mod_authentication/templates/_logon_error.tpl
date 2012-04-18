
{% if reason == "pw" %}
<h2>{_ You entered an unknown username or password.  Please try again. _}</h2>
<p>
    {_ You might have made a typo in your username or password.  Please note that both are case sensitive and check that your caps lock key is off. _}
</p>

<h3>{_ I forgot my username or password _}</h3>

<p>{_ When you forgot your username or pasword then you can ask us to _} <a href="{% url logon f="reminder" %}">{_ e-mail a temporary password _}</a>.  {_ The e-mail will contain instructions how to reset your password. _}</p>

{% elseif reason == "reminder" %}

<h2>{_ You entered an unknown username or e-mail address.  Please try again. _}</h2>
	
<p>{_ We can only send you an e-mail when we have the e-mail address of your account. _}</p>
<p>{_ To find your account	you need to enter either your username or the e-mail address you gave us. _}</p>

{% elseif reason == "tooshort" %}

<h2>{_ Your new password is too short. _}</h2>

<p>{_ Passwords should have at least six characters. _}<p>
<p>{_ Use some non alphabetical characters or digits to make it harder to guess. _}</p>

{% elseif reason == "unequal" %}

<h2>{_ The two passwords should be equal. Please retype them. _}</h2>

<p>{_ Passwords should have at least six characters. _}<p>
<p>{_ Use some non alphabetical characters or digits to make it harder to guess. _}</p>


{% endif %}


{% javascript %}
$("#logon_form form").unmask();
$("#logon_form #username").focus();
{% endjavascript %}
