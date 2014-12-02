<form id="password_reminder" method="post" action="postback">
    <div class="form-group">
        <label for="reminder_address" class="control-label">{_ E-mail address or username _}</label>
	    <input
            class="form-control"
            type="text"
            id="reminder_address"
            autofocus="autofocus="
            placeholder="{_ user@example.com _}"
            name="reminder_address"
            value="{{ q.username|default:(m.identity[m.acl.user].username)|escape }}"
            autocapitalize="off"
            autocomplete="on" />
            {% validate id="reminder_address" type={presence} only_on_submit %}
    </div>

    <div class="form-group">
        <button class="btn btn-primary" type="submit">{_ Send me instructions _}</button>
    </div>
</form>
