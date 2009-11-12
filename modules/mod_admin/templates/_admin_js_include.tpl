<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js"></script>
<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jqueryui/1.7.2/jquery-ui.min.js"></script>

{% lib 
	"js/apps/zotonic-1.0.js"
	"js/apps/z.widgetmanager.js"
	"js/apps/admin-common.js"
	
	"js/modules/z.notice.js"
	"js/modules/z.unlink.js"
	"js/modules/z.tooltip.js"
	"js/modules/z.blockminifier.js"
	"js/modules/z.dialog.js"
	"js/modules/z.formreplace.js"
	"js/modules/livevalidation-1.3.js"
%}

<script type="text/javascript">
	$(function()
	{
	    $.widgetManager();
	});
</script>