{% lib 
    "js/tinymce-3.5.0/tiny-init.js"
    "js/tinymce-3.5.0/z_editor.js"
%}
<script type="text/javascript" src="/lib/js/tinymce-3.5.0/tinymce/tiny_mce.js"></script>
<script type="text/javascript" src="/lib/js/tinymce-3.5.0/tinymce/jquery.tinymce.js"></script>
{% if not is_editor_include %}
<script type="text/javascript">
    $(document).ready(function() {
        {% all include overrides_tpl id %}
        z_editor_init();
    });
</script>
{% endif %}