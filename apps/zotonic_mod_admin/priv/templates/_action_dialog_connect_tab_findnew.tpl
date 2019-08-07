<div class="tab-pane {% if is_active %}active{% endif %}" id="{{ tab }}-findnew">
    {% include "_action_dialog_new_rsc_tab.tpl"
        delegate=delegate|default:"action_admin_dialog_new_rsc"
        subject_id=subject_id
        object_id=object_id
        predicate=predicate
        callback=callback
        cat=cat
        nocatselect=nocatselect
    %}
</div>
