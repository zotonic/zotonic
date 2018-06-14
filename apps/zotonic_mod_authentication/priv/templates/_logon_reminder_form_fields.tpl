<div class="form-group">
    <label for="reminder_address" class="control-label">{_ Your email or username _}</label>
    <input
        class="form-control"
        type="email"
        id="reminder_address"
        autofocus
        placeholder="{_ user@example.com _}"
        name="reminder_address"
        value="{% if q.email %}{{ q.email|escape }}{% else %}{{ m.acl.user.email }}{% endif %}"
        autocapitalize="off"
        autocomplete="email" />
        {% validate id="reminder_address"
            type={presence failure_message=_"Enter your email address"}
            type={email}
            only_on_submit
        %}
</div>

<div class="form-group">
    <button class="btn btn-primary" type="submit">{_ Send _}</button>
</div>
