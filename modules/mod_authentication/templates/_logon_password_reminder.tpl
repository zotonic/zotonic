<form id="password_reminder" method="post" action="postback">
    <h1 class="logon_header">{_ Forgot your password? _}</h1>

    <p>{_ Enter your e-mail address or username below and we will send you an e-mail with password reset instructions. _}</p>

    <div class="control-group">
        <label for="reminder_address" class="control-label">{_ E-mail address or username _}</label>
        <div class="controls">
	    <input type="text" id="reminder_address"
                   autofocus="autofocus="
                   class="span4"
                   placeholder="{_ user@example.com _}"
                   name="reminder_address"
                   value="{{ q.username|default:(m.identity[m.acl.user].username)|escape }}"
                   autocapitalize="off"
                   autocomplete="on" />
            {% validate id="reminder_address" type={presence} %}
        </div>
    </div>

    <div class="control-group buttons">
	<button class="btn btn-primary" type="submit">{_ Send me instructions _}</button>
	<a class="btn" href="{% url logon %}">{_ Back to logon form _}</a>
    </div>
</form>
