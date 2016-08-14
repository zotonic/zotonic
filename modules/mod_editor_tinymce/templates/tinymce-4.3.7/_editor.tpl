{% lib
    "js/tinymce-4.3.7/tiny-init.js"
    "js/tinymce-4.3.7/z_editor.js"
%}
<script type="text/javascript" src="/lib/js/tinymce-4.3.7/tinymce/tinymce.min.js"></script>
<script type="text/javascript" src="/lib/js/tinymce-4.3.7/tinymce/jquery.tinymce.min.js"></script>

{% if not is_editor_include %}
<script type="text/javascript">
    $(document).ready(function() {
        {% all include overrides_tpl id %}
        z_editor_init();
    });
</script>
{% endif %}
