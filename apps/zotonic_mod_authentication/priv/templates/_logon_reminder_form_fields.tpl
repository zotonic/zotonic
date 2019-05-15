<div class="form-group">
    <label for="reminder_address" class="control-label">{_ Your email _}</label>
    <input
        class="form-control"
        type="email"
        name="email"
        id="reminder_address"
        placeholder="{_ user@example.com _}"
        value="{% if q.email %}{{ q.email|escape }}{% else %}{{ m.acl.user.email }}{% endif %}"
        autocapitalize="off"
        autocomplete="email"
        autofocus
        required />
</div>

<div class="form-group">
    <button class="btn btn-primary" type="submit">{_ Send _}</button>
</div>
