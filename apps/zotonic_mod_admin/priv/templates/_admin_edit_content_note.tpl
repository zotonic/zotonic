{% if id.is_editable and m.acl.is_allowed.use.mod_admin %}
<div id="admin-rsc-note">
    {% include "_admin_edit_content_note_inner.tpl" %}
</div>
{% endif %}
