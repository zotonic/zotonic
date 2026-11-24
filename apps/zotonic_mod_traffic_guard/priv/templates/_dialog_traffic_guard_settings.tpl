{% if m.acl.is_admin and not m.acl.is_read_only %}
{% wire id=#settings type="submit" postback="save_settings" delegate="controller_admin_traffic_guard" %}
<form id="{{ #settings }}" method="post" action="postback">
    <div class="modal-body">
        <div class="form-group">
            <label for="{{ #mode }}">{_ Mode _}</label>
            <select id="{{ #mode }}" name="mode" class="form-control" required>
                <option value="off" {% if settings.mode == `off` %}selected{% endif %}>{_ Off — bypass traffic guard _}</option>
                <option value="observe" {% if settings.mode == `observe` %}selected{% endif %}>{_ Observe — report without blocking _}</option>
                <option value="enforce" {% if settings.mode == `enforce` %}selected{% endif %}>{_ Enforce — reject excess requests _}</option>
            </select>
            <p class="help-block">{_ Default: observe. Use observe mode to tune limits against normal traffic. _}</p>
        </div>
        <div class="form-group">
            <label for="{{ #period }}">{_ Replenishment period (seconds) _}</label>
            <input id="{{ #period }}" name="period" type="number" class="form-control"
                   min="1" max="3600" step="1" required value="{{ settings.period|escape }}">
            <p class="help-block">{_ 1–3600 seconds. Default: 60. _}</p>
        </div>
        <div class="form-group">
            <label for="{{ #request_limit }}">{_ Requests replenished per period _}</label>
            <input id="{{ #request_limit }}" name="request_limit" type="number" class="form-control"
                   min="1" max="100000" step="1" required value="{{ settings.request_limit|escape }}">
            <p class="help-block">{_ 1–100000 requests per period. Default: 120. Credits replenish continuously for each IP address. _}</p>
        </div>
        <div class="form-group">
            <label for="{{ #request_burst }}">{_ Burst allowance _}</label>
            <input id="{{ #request_burst }}" name="request_burst" type="number" class="form-control"
                   min="1" max="1000" step="1" required value="{{ settings.request_burst|escape }}">
            <p class="help-block">{_ 1–1000 immediately available request credits per IP address. Default: 30. _}</p>
        </div>
        <div class="form-group">
            <label for="{{ #max_clients }}">{_ Maximum tracked clients _}</label>
            <input id="{{ #max_clients }}" name="max_clients" type="number" class="form-control"
                   min="1" max="50000" step="1" required value="{{ settings.max_clients|escape }}">
            <p class="help-block">{_ 1–50000 retained client budgets per site and node. Default: 5000. _}</p>
        </div>
        <p class="help-block">{_ Saving settings resets transient request budgets. IP allow and block entries are managed separately on the Traffic guard screen. _}</p>
    </div>
    <div class="modal-footer">
        {% button text=_"Cancel" class="btn btn-default" action={dialog_close} %}
        <button type="submit" class="btn btn-primary">{_ Save settings _}</button>
    </div>
</form>
{% endif %}
