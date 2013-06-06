<script type="text/javascript" src="/lib/js/modules/tinymce3.5.0/tiny_mce.js"></script>
<script type="text/javascript" src="/lib/js/modules/tinymce3.5.0/jquery.tinymce.js"></script>
<script type="text/javascript">
$(document).ready(function(){
	{% all catinclude "_admin_tinymce_overrides_js.tpl" id %}
    z_tinymce_init();
});
</script>
