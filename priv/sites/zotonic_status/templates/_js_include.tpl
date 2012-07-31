{% include "_js_include_jquery.tpl" %}
{% lib 
	"bootstrap/js/bootstrap.min.js"
	"js/apps/zotonic-1.0.js"
	"js/apps/z.widgetmanager.js"
	"js/modules/livevalidation-1.3.js"
	"js/modules/z.inputoverlay.js"
%}

{% stream %}
{% script %}

<script type="text/javascript">
	$(function()
	{
	    $.widgetManager();
	});
</script>
