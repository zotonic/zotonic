{% lib 
    "js/tinymce-4.2.4/tiny-init.js"
    "js/tinymce-4.2.4/z_editor.js"
%}
<script type="text/javascript" src="/lib/js/tinymce-4.2.4/tinymce/tinymce.min.js"></script>
<script type="text/javascript" src="/lib/js/tinymce-4.2.4/tinymce/jquery.tinymce.min.js"></script>

{% if not is_editor_include %}
<script type="text/javascript">
    $(document).ready(function() {
        {% all include overrides_tpl id %}
        z_editor_init();
    });
</script>
{% endif %}
