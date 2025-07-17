{% if is_nocatview %}
    <input type="hidden" id="{{ #catsel }}" name="category_id" value="{{ cat_id }}"/>
{% else %}
    <div class="form-group">
        <label class="control-label" for="{{ #category }}">{_ Category _}</label>
        {% if is_nocatselect %}
            <input class="form-control nosubmit" type="text" readonly value="{{ cat_id.title }}" />
            <input type="hidden" id="{{ #catsel }}" name="category_id" value="{{ cat_id }}"/>
        {% else %}
            {% include "_admin_category_dropdown.tpl" catsel_id=#catsel %}
        {% endif %}
    </div>
{% endif %}

<div class="form-group">
    <label class="control-label" for="{{ #category }}">{_ Content group _}</label>

    <div id="{{ #cgwrapper }}">
        {% include "_admin_content_group_dropdown.tpl" cgsel_id=#cgsel %}

        {% if not no_collab and m.search::%{ cat: 'acl_collaboration_group', pagelen: 1 } %}
            <br/>
            <button type="button" class="btn btn-default" id="{{ #collab_select }}">
                {% trans "Move to {title}…" title=m.rsc.acl_collaboration_group.title %}
            </button>

            {% wire id=#collab_select
                    action={dialog_open
                            intent="select"
                            template="_action_dialog_connect.tpl"
                            subject_id=id
                            title=[_"Select", " ", m.rsc.acl_collaboration_group.title]
                            category=`acl_collaboration_group`
                            tabs_enabled=["find"]
                            delegate=`admin_acl_rules`
                            nocatselect
                            autoclose
                        }
            %}
        {% endif %}
    </div>
</div>

<div id="{{ error|default:#error }}" class="form-group" style="display: none">
    <div class="alert alert-danger">
        {_ Sorry, you are not allowed to insert pages of this category into the selected content group. _}<br/>
        {_ Try a different combination of category and content group. _}
    </div>
</div>

{% if is_nocatselect %}
    {% javascript %}
        if (!$('#{{ #cgsel }}').val()) {
            $('#{{ error|default:#error }}').show();
        }
    {% endjavascript %}
{% else %}
    {% javascript %}
        {
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
                                cgsel: "{{ #cgsel }}",
                                subject_id: '{{ subject_id }}'
                            });
            });

            const cats = $('#{{ #catsel }} option:enabled');
            const cgs = $('#{{ #cgsel }} option:enabled');

            if (!$('#{{ #catsel }}').val() && cats.length == 1) {
                $('#{{ #catsel }}').val(cats[0].value);
                $('#{{ #catsel }}').trigger('change');
            } else if ($('#{{ #catsel }}').val() && cgs.length >= 1 && !$('#{{ #cgsel }}').val()) {
                $('#{{ #catsel }}').trigger('change');
            }
        }
    {% endjavascript %}
{% endif %}
