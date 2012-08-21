
{% include "_js_include_jquery.tpl" %}

{% lib
	"js/apps/zotonic-1.0.js"
	"js/apps/z.widgetmanager.js"
	"js/modules/z.notice.js"
	"js/modules/z.imageviewer.js"
	"js/modules/z.dialog.js"
	"js/modules/livevalidation-1.3.js"
	"js/modules/z.inputoverlay.js"
	"js/modules/jquery.loadmask.js"
	"bootstrap/js/bootstrap.js"
	"js/modules/responsive.js"
%}

{% block _js_include_extra %}{% endblock %}

<script type="text/javascript">
	$(function()
	{
	    $.widgetManager();
	});
</script>
