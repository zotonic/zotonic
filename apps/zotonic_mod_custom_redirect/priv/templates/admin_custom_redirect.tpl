{% extends "admin_base.tpl" %}

{% block title %} {_ Admin Custom Redirects _} {% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ Domains and redirects _}</h2>
    <p>{_ Redirect unknown domains and paths to known pages or locations. _}</p>
    <p>{_ The new location can be a local path or a complete URL. Leave the host empty for redirects within this site. _}</p>
</div>

<div class="widget">
    <div class="widget-content">
        {% wire id=#form
                type="submit"
                postback=`custom_redirects`
                delegate=`mod_custom_redirect`
        %}
        <form id="{{ #form }}" action="postback">
            <table id="custom-redirects" class="table table-striped do_adminLinkedTable">
                <thead>
                    <tr>
                        <th>{_ Domain _}</th>
                        <th>{_ Path _}</th>
                        <th>{_ Redirect to _}</th>
                        <th>{_ Permanent _}</th>
                        <th>{_ Actions _}</th>
                    </tr>
                </thead>

                <tbody>
                    {% for r in m.custom_redirect.list %}
                    <tr>
                        <td>
                            <input type="hidden" name="id" value="{{ r.id }}">
                            <input type="text" class="input form-control" name="host" value="{{ r.host|escape }}" maxlength="250">
                        </td>
                        <td>
                            <input type="text" class="input form-control" name="path" value="{{ r.path|escape }}" maxlength="1000">
                        </td>
                        <td>
                            <input type="text" class="input col-lg-5 col-md-5 form-control" name="redirect" value="{{ r.redirect|escape }}" maxlength="1000">
                        </td>
                        <td>
                            <div class="checkbox">
                                <label>
                                    <input type="checkbox" name="is_permanent" value="1" {% if r.is_permanent %}checked{% endif %}> {_ Permanent _}
                                </label>
                            </div>
                        </td>
                        <td>
                            <a href="#delete" class="btn btn-default delete">{_ Delete _}</a>
                        </td>
                    </tr>
                    {% endfor %}
                    <tr>
                        <td>
                            <input type="hidden" name="id" value="">
                            <input type="text" class="input form-control" name="host" placeholder="www.example.com" value="" maxlength="250">
                        </td>
                        <td>
                            <input type="text" class="input form-control" name="path" value="" maxlength="1000">
                        </td>
                        <td>
                            <input type="text" class="input col-lg-5 col-md-5 form-control" name="redirect" value="" maxlength="1000">
                        </td>
                        <td>
                            <div class="checkbox">
                                <label>
                                    <input type="checkbox" name="is_permanent" value="1" > {_ Permanent _}
                                </label>
                            </div>
                       </td>
                        <td>
                            <a href="#delete" class="btn btn-default delete">{_ Delete _}</a>
                        </td>
                    </tr>
                    <tr id="template" style="display: none">
                        <td>
                            <input type="hidden" name="id" value="">
                            <input type="text" class="input form-control" name="host" placeholder="www.example.com" value="" maxlength="250">
                        </td>
                        <td>
                            <input type="text" class="input form-control" name="path" value="" maxlength="1000">
                        </td>
                        <td>
                            <input type="text" class="input col-lg-5 col-md-5 form-control" name="redirect" value="" maxlength="1000">
                        </td>
                        <td>
                            <div class="checkbox">
                                <label>
                                    <input type="checkbox" name="is_permanent" value="1" > {_ Permanent _}
                                </label>
                            </div>
                       </td>
                        <td>
                            <a href="#delete" class="btn btn-default delete">{_ Delete _}</a>
                        </td>
                    </tr>
                </tbody>
            </table>

            <div class="form-actions">
                <a id="{{ #cancel }}" class="btn btn-default">{_ Cancel _}</a>
                <button type="submit" class="btn btn-primary">{_ Save _}</button>
                {% wire id=#cancel action={reload} %}
            </div>
        </form>
    </div>
</div>

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
