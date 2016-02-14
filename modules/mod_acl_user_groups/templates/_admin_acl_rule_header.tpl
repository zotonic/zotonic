<div class="row acl-header">

    <div class="col-md-1">
        <label>{_ Deny _}</label>
    </div>

    <div class="col-md-2">
        <label>{_ ACL user group _}</label>
    </div>

    {% if kind == "rsc" %}
        <div class="col-md-2">
            <label>{_ Content group _}</label>
        </div>

        <div class="col-md-2">
            <label>{_ Category _}</label>
        </div>

    {% elseif kind == "module" %}
        <div class="col-md-4">
            <label>{_ Module _}</label>
        </div>
    {% endif %}

    <div class="col-md-5">
        <label>{_ Permissions _}</label>
    </div>

</div>
