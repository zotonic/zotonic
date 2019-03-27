{% if q.error == "pw" %}
    <p>
        {_ Either the email or the password you entered is incorrect. Please check your entry and try again. _}
    </p>

    <p>
        <a href="{% url logon_reminder %}" id="logon_error_link_reminder" data-onclick-topic="model/auth-ui/post/view/reminder">
            {_ Need help signing in? _}
        </a>
    </p>

{% elseif q.error == "reminder" %}

    <p>{_ You've entered an unknown username or email address. Please try again. _}</p>

    <p>{_ We can only send you an email when we have the email address of your account. _}</p>
    <p>{_ To find your account you need to enter either your username or the email address we have received from you. _}</p>

{% elseif q.error == "tooshort" %}

    <p>{_ Your new password is too short. _}</p>

    <p>{_ Passwords should have at least six characters. _}</p>
    <p>{_ Use some non alphabetical characters or digits to make it harder to guess. _}</p>

{% elseif q.error == "unequal" %}

    <p>{_ The two passwords should be equal. Please retype them. _}</p>

    <p>{_ Passwords should have at least six characters. _}</p>
    <p>{_ Use some non alphabetical characters or digits to make it harder to guess. _}</p>

{% else %}

    <p>{_ Something went wrong, please try again later. _}</p>

{% endif %}
