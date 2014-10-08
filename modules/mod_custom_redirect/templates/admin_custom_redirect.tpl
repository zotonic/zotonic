{% extends "admin_base.tpl" %}

{% block title %} {_ Admin Custom Redirects _} {% endblock %}

{% block content %}
<div class="edit-header">
    <h2>{_ Domains and redirects _}</h2>
    <p>{_ Redirect unknown domains and paths to known pages or locations. _}</p>
    <p>{_ The new location can be a local path or a complete URL. Leave the host empty for redirects within this site. _}</p>
</div>

{% wire id=#form
        type="submit"
        postback=`custom_redirects`
        delegate=`mod_custom_redirect`
%}
<form id="{{ #form }}" action="#postback">
    <table id="custom-redirects" class="table table-striped do_adminLinkedTable">
        <thead>
            <tr>
                <th>{_ Domain _}</th>
                <th>{_ Path _}</th>
                <th>{_ Redirect to _}</th>
                <th width="5%">{_ Permanent _}</th>
                <th width="5%">{_ Actions _}</th>
            </tr>
        </thead>

        <tbody>
            {% for r in m.custom_redirect.list %}
            <tr>
                <td>
                    <input type="hidden" name="id" value="{{ r.id }}" />
                    <input type="text" class="input form-control" name="host" value="{{ r.host|escape }}" />
                </td>
                <td>
                    <input type="text" class="input form-control" name="path" value="{{ r.path|escape }}" />
                </td>
                <td>
                    <input type="text" class="input col-lg-5 col-md-5 form-control" name="redirect" value="{{ r.redirect|escape }}" />
                </td>
                <td>
                    <div class="checkbox"><label>
                        <input type="checkbox" name="is_permanent" value="1" {% if r.is_permanent %}checked{% endif %} />
                        {_ Permanent _}
                    </label></div>
                </td>
                <td>
                    <a href="#delete" class="btn btn-default delete">{_ Delete _}</a>
                </td>
            </tr>
            {% endfor %}
            <tr>
                <td>
                    <input type="hidden" name="id" value="" />
                    <input type="text" class="input form-control" name="host" placeholder="www.example.com" value="" />
                </td>
                <td>
                    <input type="text" class="input form-control" name="path" value="" />
                </td>
                <td>
                    <input type="text" class="input col-lg-5 col-md-5 form-control" name="redirect" value="" />
                </td>
                <td>
                    <div class="checkbox"><label>
                        <input type="checkbox" name="is_permanent" value="1" />
                        {_ Permanent _}
                    </label></div>
               </td>
                <td>
                    <a href="#delete" class="btn btn-default delete">{_ Delete _}</a>
                </td>
            </tr>
            <tr id="template" style="display: none">
                <td>
                    <input type="hidden" name="id" value="" />
                    <input type="text" class="input form-control" name="host" placeholder="www.example.com" value="" />
                </td>
                <td>
                    <input type="text" class="input form-control" name="path" value="" />
                </td>
                <td>
                    <input type="text" class="input col-lg-5 col-md-5 form-control" name="redirect" value="" />
                </td>
                <td>
                    <div class="checkbox"><label>
                        <input type="checkbox" name="is_permanent" value="1" />
                        {_ Permanent _}
                    </label></div>
               </td>
                <td>
                    <a href="#delete" class="btn btn-default delete">{_ Delete _}</a>
                </td>
            </tr>
        </tbody>
    </table>

    <div class="form-actions">
        <button type="submit" class="btn btn-primary">{_ Save _}</button>
        <a id="{{ #cancel }}" class="btn btn-default">{_ Cancel _}</a>
        {% wire id=#cancel action={reload} %}
    </div>
</form>

{% javascript %}
$('#custom-redirects').on('keyup', function() {
    $(this).trigger("check-placeholder"); 
});
$('#custom-redirects').on('click', '.delete', function(e) {
    $(this).closest('tr').fadeOut('normal', function() { 
        $(this).remove(); 
        $('#custom-redirects ').trigger("check-placeholder");
    });
    e.preventDefault();
});

$('#custom-redirects ').on('check-placeholder', function() {
    var inputs = $("tbody tr:visible:last input.input", this);
    var is_empty = inputs.length > 0;
    inputs.each(function() {
        if ($(this).val() != '') {
            is_empty = false;
        }
    });
    if (!is_empty) {
        $('#template')
            .clone()
            .removeAttr('id')
            .removeAttr('style')
            .insertBefore('#template');
    }
});
{% endjavascript %}

{% endblock %}
