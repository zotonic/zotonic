{% lib
    "js/tinymce-4.5.5/tiny-init.js"
    "js/tinymce-4.5.5/z_editor.js"
%}
<script type="text/javascript" src="/lib/js/tinymce-4.5.5/tinymce/tinymce.min.js"></script>
<script type="text/javascript" src="/lib/js/tinymce-4.5.5/tinymce/jquery.tinymce.min.js"></script>

{% if not is_editor_include %}
<script type="text/javascript">
    $(document).ready(function() {
        {% all include overrides_tpl id %}
        z_editor_init();
    });
</script>
{% endif %}
