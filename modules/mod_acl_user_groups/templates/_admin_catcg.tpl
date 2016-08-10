<div class="form-group row">
    <label class="control-label col-md-3" for="{{ #category }}">{_ Category _}</label>
    <div class="col-md-9">
        {% if is_nocatselect %}
            <input class="form-control" type="text" readonly value="{{ cat_id.title }}" />
            <input type="hidden" id="{{ #catsel }}" name="category_id" value="{{ cat_id }}"/>
        {% else %}
            {% include "_admin_category_dropdown.tpl" catsel_id=#catsel %}
        {% endif %}
    </div>
</div>

<div class="form-group row">
    <label class="control-label col-md-3" for="{{ #category }}">{_ Content group _}</label>
    <div class="col-md-9" id="{{ #cgwrapper }}">
        {% include "_admin_content_group_dropdown.tpl" cgsel_id=#cgsel %}

        <br/>
        <a href="#" class="btn btn-default" id="{{ #collab_select }}">
            {_ Move to another _} {{ m.rsc.acl_collaboration_group.title }} …
        </a>

        {% wire id=#collab_select
                action={dialog_open
                        subject_id=id
                        template="_action_dialog_connect.tpl"
                        title=[_"Move to another", " ", m.rsc.acl_collaboration_group.title]
                        category=`acl_collaboration_group`
                        tabs_enabled=["find"]
                        delegate=`admin_acl_rules`
                        nocatselect
                        autoclose
                    }
        %}
    </div>
</div>

<div id="{{ error|default:#error }}" class="form-group row" style="display: none">
    <div class="col-md-9 col-md-offset-3">
        <div class="alert alert-danger">
            {_ Sorry, you are not allowed to insert pages of this category into the selected content group. _}<br/>
            {_ Try a different combination of category and content group. _}
        </div>
    </div>
</div>

{% if is_notcatselect %}
    {% javascript %}
        if (!$('#{{ #cgsel }}').val()) {
            $('#{{ error|default:#error }}').show();
        }
    {% endjavascript %}
{% else %}
    {% javascript %}
        $('#{{ #catsel }}').change(function() {
            $('#{{ #cgsel }}').mask();
            z_transport("admin_acl_rules_rsc",
                        "ubf",
                        {
                            cmd: "reload_cgsel",
                            cat_id: $(this).val(),
                            cg_id1: $('#{{ #cgsel }}').val(),
                            cg_id2: '{{ cg_id|default:id.content_group_id }}',
                            cgwrap: "{{ #cgwrapper }}",
                            cgsel: "{{ #cgsel }}"
                        });
        });

        if ($('#{{ #catsel }}').val() && !$('#{{ #cgsel }}').val()) {
            $('#{{ #catsel }}').trigger('change');
        }
    {% endjavascript %}
{% endif %}
