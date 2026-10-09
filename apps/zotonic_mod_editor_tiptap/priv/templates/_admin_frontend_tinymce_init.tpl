{# Compatibility hook from mod_admin_frontend; deliberately loads no TinyMCE. #}
{% javascript %}
    {% all include "_editor_tiptap_overrides_js.tpl" id=id %}
{% endjavascript %}
