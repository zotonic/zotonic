{% if id.is_visible and not id.is_authoritative %}
    <div class="alert alert-warning" id="non-authoritative">
        <span class="glyphicon glyphicon-info-sign"></span>
        {_ This page is from another website. The original can be found at _}
        <a href="{{ id.uri|escape }}" target="blank" rel="noopener noreferrer">{{ id.uri|escape }}</a>

        {% if id.is_editable %}
            <span class="pull-right">
                <button class="btn btn-default btn-xs" id="{{ #refresh }}">{_ Fetch new version _}</button>
                {% wire id=#refresh
                    postback={import_options id=id}
                    delegate=`z_admin_rsc_import`
                %}
            </span>
            {% all include "_admin_rsc_import_status.tpl" id=id %}
        {% endif %}
    </div>
{% endif %}
