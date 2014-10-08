<form id="password_reminder" method="post" action="postback">
    <h1 class="logon_header">{_ Forgot your password? _}</h1>

    <p>{_ Enter your e-mail address or username below and we will send you an e-mail with password reset instructions. _}</p>

    <div class="form-group">
        <label for="reminder_address" class="control-label">{_ E-mail address or username _}</label>
        <div>
	    <input class="form-control" type="text" id="reminder_address"
                   autofocus="autofocus="
                   placeholder="{_ user@example.com _}"
                   name="reminder_address"
                   value="{{ q.username|default:(m.identity[m.acl.user].username)|escape }}"
                   autocapitalize="off"
                   autocomplete="on" />
            {% validate id="reminder_address" type={presence} %}
        </div>
    </div>

    <div class="form-group buttons">
        <button class="btn btn-primary" type="submit">{_ Send me instructions _}</button>
        {% if not m.acl.user %}
	       <a class="btn btn-default" href="{% url logon %}">{_ Back to logon form _}</a>
        {% endif %}
    </div>
</form>
