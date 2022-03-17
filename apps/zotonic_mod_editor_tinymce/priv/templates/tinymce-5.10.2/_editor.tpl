{% lib
    "js/tinymce-5.10.2/tiny-init.js"
    "js/tinymce-5.10.2/z_editor.js"
%}
<script type="text/javascript" src="/lib/js/tinymce-5.10.2/tinymce/tinymce.min.js"></script>
<script type="text/javascript" src="/lib/js/tinymce-5.10.2/tinymce/jquery.tinymce.min.js"></script>

{% if not is_editor_include %}
<script type="text/javascript" nonce="{{ m.req.csp_nonce }}">
    $(document).ready(function() {
        {% all include overrides_tpl id %}
        z_editor_init();
    });
</script>
{% endif %}
