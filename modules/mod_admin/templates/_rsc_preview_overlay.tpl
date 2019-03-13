{% with m.rsc[id|default:q.select_id].id as id %}
<div class="rsc-preview-wrapper">
    <div class="rsc-preview">
        {% catinclude "_rsc_preview.tpl" id
            subject_id=subject_id
            object_id=object_id
            predicate=predicate
            callback=callback
            language=language
            action=action
            actions=actions
            autoclose=autoclose
        %}
    </div>
</div>
{% endwith %}
