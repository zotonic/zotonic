<script type="text/javascript" src="/lib/js/modules/tinymce3.5.0/tiny_mce.js"></script>
<script type="text/javascript" src="/lib/js/modules/tinymce3.5.0/jquery.tinymce.js"></script>
{% if not is_tinymce_include %}
<script type="text/javascript">
$(document).ready(function(){
	{% all catinclude "_admin_tinymce_overrides_js.tpl" id %}
    z_tinymce_init();
});
</script>
{% endif %}
