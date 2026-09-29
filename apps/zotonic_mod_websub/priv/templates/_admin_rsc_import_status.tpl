{% if id.is_editable and not id.is_authoritative %}
    {% live template="_admin_rsc_import_status_live.tpl"
            id=id
            topic=["bridge", "origin", "model", "websub", "event", "rsc", id]
    %}
{% endif %}
