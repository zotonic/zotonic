{#
    Preview panel for connect/new dialog.

    Arguments:

        target=#view
        newform=#newform
        template="_rsc_preview_panel.tpl"
        id=id
        subject_id=subject_id
        object_id=object_id
        predicate=predicate
        callback=callback
        language=language
        action=action
        actions=actions
        autoclose=autoclose
        is_zlink
#}

{% with m.rsc[q.select_id].id as id %}
{% with (subject_id and m.acl.is_allowed.link[subject_id])
     or (object_id and m.acl.is_allowed.link[id])
   as is_linkable
%}

<div class="rsc-preview-panel" data-id="{{ id }}">
    <div class="rsc-preview">
        {% catinclude "_rsc_preview.tpl" id %}
    </div>
    <div class="rsc-preview-action modal-footer">
        <a href="#" id="{{ #cancel }}" class="btn btn-default">{_ Close _}</a>
        {% wire id=#cancel action={trigger_event name='dialog_new_rsc_preview_close'} %}

        {% if id.is_editable %}
            {% with not is_zlink and not predicate as is_primary %}
                <a href="#" class="btn {% if is_primary %}btn-primary{% else %}btn-default{% endif %} action-edit">
                    {_ Visit full edit page _}
                </a>
            {% endwith %}
        {% endif %}

        {% if is_zlink %}
            <a href="#" class="btn btn-primary action-connect">{_ Link _}</a>
        {% elseif predicate and is_linkable %}
            <a href="#" class="btn btn-primary action-connect">{_ Connect _}</a>
        {% endif %}
    </div>
</div>

{% endwith %}
{% endwith %}
