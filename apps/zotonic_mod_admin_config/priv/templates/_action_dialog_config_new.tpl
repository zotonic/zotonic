<p>{_ Please fill in the module, key and value for the new configuration key. _}</p>

{% wire id=#form type="submit" postback={config_new on_success=on_success} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback" class="form">

    <div class="row">
        <div class="form-group label-floating col-sm-6">
            <input type="text" id="{{ #module }}" name="module" value="" class="form-control" required placeholder="{_ Module _}" pattern="[a-zA-Z0-9_]+">
    	    <label class="control-label" for="{{ #module }}">{_ Module _}</label>
            {% validate id=#module name="module" type={presence} %}
        </div>
        <div class="form-group label-floating col-sm-6">
            <input class="form-control" type="text" id="{{ #key }}" name="key" value="" required pattern="[a-zA-Z0-9_]+" placeholder="{_ Key _}">
            <label class="control-label" for="{{ #key }}">{_ Key _}</label>
            {% validate id=#key name="key" type={presence} %}
        </div>
    </div>

    <div class="form-group label-floating">
        <input class="form-control" type="text" id="{{ #value }}" name="val" value="" placeholder="{_ Value _}">
        <label class="control-label" for="{{ #value }}">{_ Value _}</label>
    </div>

    <div class="modal-footer">
        {% button class="btn btn-default" action={dialog_close} text="Cancel" tag="a" %}
        <button class="btn btn-primary" type="submit">{_ Add key _}</button>
    </div>
</form>

<br>

<input type="text"
       autofocus
       class="form-control do_listfilter do_autofocus"
       placeholder="{_ Type to find configuration keys... _}"
       data-listfilter='{ "method": "words", "list": "#available-configs tr" }'
>

<table class="table table-compact">
    <thead>
        <tr>
            <th>{_ Module _}</th>
            <th>{_ Key _}</th>
            <th>{_ Default _}</th>
            <th>{_ Description _}</th>
        </tr>
    </thead>
    <tbody id="available-configs">
        {% for config in m.admin_config.configs %}
            <tr class="clickable">
                <td>{{ config.module|escape }}</td>
                <td>{{ config.key|escape }}</td>
                <td><span class="text-muted">{{ config.default|escape }}</span></td>
                <td>{{ config.description }}</td>
            </tr>
        {% empty %}
            <tr>
                <td colspan="4"><span class="text-muted">{_ No configuration keys found. _}</span></td>
            </tr>
        {% endfor %}
    </tbody>
</table>

{% javascript %}
    $('#available-configs').on('click', 'tr', function() {
        const $tds = $(this).children('td');
        const module = $tds.eq(0).text().trim();
        const key = $tds.eq(1).text().trim();
        const value = $tds.eq(2).text().trim();

        $('#{{ #module }}').val(module);
        $('#{{ #key }}').val(key);
        $('#{{ #value }}').val(value);

        $(this).closest('.modal-content').get(0).scrollIntoView({ behavior: 'smooth' });
        $('#{{ #value }}').select().focus();
    });
{% endjavascript %}
