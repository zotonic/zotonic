{% lib
    "js/tinymce-4.9.3/tiny-init.js"
    "js/tinymce-4.9.3/z_editor.js"
%}
<script type="text/javascript" src="/lib/js/tinymce-4.9.3/tinymce/tinymce.min.js" nonce="{{ m.req.csp_nonce }}"></script>
<script type="text/javascript" src="/lib/js/tinymce-4.9.3/tinymce/jquery.tinymce.min.js" nonce="{{ m.req.csp_nonce }}"></script>

{% if not is_editor_include %}
<script type="text/javascript" nonce="{{ m.req.csp_nonce }}">
    $(document).ready(function() {
        {% all include overrides_tpl id %}
        z_editor_init();
    });
</script>
{% endif %}
