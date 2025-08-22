{% wire id=#form type="submit" postback={config_edit module=module key=key on_success=on_success} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback" class="form">
    <div class="row">
        <div class="form-group label-floating col-sm-6">
            <input type="text" id="{{ #module }}" name="module" value="{{ module|escape }}" class="form-control"  required placeholder="{_ Module _}" pattern="[a-zA-Z0-9_]+">
            <label class="control-label" for="{{ #module }}">{_ Module _}</label>
            {% validate id=#module name="module" type={presence} %}
        </div>

        <div class="form-group label-floating col-sm-6">
            <input class="form-control" type="text" id="{{ #key }}" name="key" value="{{ key|escape }}" required pattern="[a-zA-Z0-9_]+" placeholder="{_ Key _}">
            <label class="control-label" for="{{ #key }}">{_ Key _}</label>
            {% validate id=#key name="key" type={presence} %}
        </div>
    </div>

    <div class="form-group label-floating">
        <input class="form-control" type="text" id="{{ #value }}" name="val" value="{{ value|escape }}" placeholder="{_ Value _}" autofocus>
        <label class="control-label" for="{{ #value }}">{_ Value _}</label>
    </div>

    {% if m.admin_config.config[module][key] as c %}
        <p class="help-block">
            {{ c.description }}
            {% if c.default|length > 0 %}
                <br>{_ Default _}: <b>{{ c.default|escape }}</b>
            {% endif %}
        </p>
    {% endif %}

    <div class="modal-footer">
	    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
        <button class="btn btn-primary" type="submit">{_ Save _}</button>
    </div>
</form>
