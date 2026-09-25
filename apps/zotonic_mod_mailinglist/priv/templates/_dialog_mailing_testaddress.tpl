
{% wire type="submit"
        id=#form
        postback={mailing_testaddress id=id}
        action={dialog_close}
        delegate="mod_mailinglist"
%}
<form id="{{ #form }}" method="post" action="postback">

    <p>
        {_ Please enter the e-mail address you want to send a test mail to. _}
        {_ The test will be sent immediately, even if the page is not published. _}
    </p>

    <div class="form-group label-floating">
        <input id="{{ #email }}" type="email" value="{{ m.acl.user.email }}" class="form-control" autofocus name="email" placeholder="{_ E-mail _}">
        <label class="control-label" for="email">{_ E-mail _}</label>
        {% validate id=#email name="email" type={presence} type={email} %}
    </div>
    <div class="form-group">
        <label for="{{ #language }}">{_ Language _}</label>
        <select id="{{ #language }}" name="mailing_language" class="form-control">
            {% for code in id.language %}
                <option value="{{ code|escape }}">
                    {{ m.translation.language_list_configured[code].name|default:code|escape }}
                </option>
            {% empty %}
                <option value="{{ z_language|escape }}">{{ z_language|escape }}</option>
            {% endfor %}
        </select>
    </div>
    <div class="modal-footer">
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} %}
        {% button class="btn btn-primary" type="submit" text=_"Send immediately" %}
    </div>
</form>
