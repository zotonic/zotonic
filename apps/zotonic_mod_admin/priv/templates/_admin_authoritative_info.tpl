{% if id.is_visible and not id.is_authoritative %}
    <div class="alert alert-warning">
        <span class="glyphicon glyphicon-info-sign"></span>
        {_ This page is from another website. The original can be found at _}
        <a href="{{ id.uri|escape }}" target="blank" rel="noopener noreferrer">{{ id.uri|escape }}</a>

        {% if id.is_editable %}
            <span class="pull-right">
                <button class="btn btn-default btn-xs" id="{{ #refresh }}">{_ Refresh _}</button>
                {% wire id=#refresh
                        postback={import_refresh id=id on_success={reload} }
                        delegate=`z_admin_rsc_import`
                %}
            </span>
        {% endif %}
    </div>
{% endif %}
