
{###################################}
{# Show / hide logon form elements #}
{###################################}

{% wire action={unmask target=" form"} %}

{% if reason == "pw" %}
    {% wire action={focus target="username"} %}
{% elseif reason == "need_passcode" %}
    {% wire action={focus target="passcode"} %}
{% elseif reason == "passcode" %}
    {% wire action={focus target="passcode"} %}
{% elseif reason == "ratelimit" %}
    {# nothing #}
{% elseif reason == "reminder" %}
    {% wire action={focus target="password_reset1"} %}
{% elseif reason == "tooshort" %}
    {% wire action={focus target="password_reset1"} %}
{% elseif reason == "unequal" %}
    {% wire action={focus target="password_reset1"} %}
{% elseif reason == "password_change_match" %}
    {% wire action={focus target="password_reset1"} %}
{% else %}
    {% wire action={focus target="username"} %}
{% endif %}

{% catinclude "logon_error/action.tpl" [reason|as_atom] %}

{#######################}
{# Show error messages #}
{#######################}

{% catinclude "logon_error/message.tpl" [reason|as_atom] %}
