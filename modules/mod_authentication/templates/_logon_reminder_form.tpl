{% if use_wire %}
    {#
        Use a wired postback when we are not using the default logon page with
        its logon_controller. 
    #}
    {% wire id="password_reminder" type="submit" postback={reminder} delegate=`mod_authentication` %}
{% endif %}
<form id="password_reminder" method="post" action="postback">
    <div class="form-group">
        <label for="reminder_address" class="control-label">{_ Your e-mail address or username _}</label>
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
        <button class="btn btn-primary" type="submit">{_ Request reset _}</button>
    </div>
</form>
