{% if id.is_editable and m.acl.is_allowed.use.mod_admin %}
<div id="admin-rsc-note">
    {% live template="_admin_edit_content_note_inner.tpl"
            id=id
            topic=[ "bridge", "origin", "model", "admin_note", "event", "rsc", id ]
    %}
</div>
{% endif %}
