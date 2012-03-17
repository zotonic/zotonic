{% lib 
        "bootstrap/js/bootstrap.min.js"

        "js/apps/zotonic-1.0.js"
	"js/apps/z.widgetmanager.js"
	"js/apps/admin-common.js"
	
	"js/jquery.ui.nestedSortable.js"
	
	"js/modules/z.notice.js"
	"js/modules/z.unlink.js"
	"js/modules/z.tooltip.js"
	"js/modules/z.blockminifier.js"
	"js/modules/z.dialog.js"
	"js/modules/z.formreplace.js"
	"js/modules/z.datepicker.js"
	"js/modules/z.menuedit.js"
	"js/modules/z.trash.js"
	"js/modules/livevalidation-1.3.js"
	"js/modules/jquery.loadmask.js"
%}

<script type="text/javascript">
	$(function()
	{
	    $.widgetManager();
	});
</script>
