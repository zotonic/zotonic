{% include "_js_include_jquery.tpl" %}
{% lib
	"bootstrap/js/bootstrap.min.js"
	"js/apps/zotonic-1.0.js"
	"js/apps/z.widgetmanager.js"

    "js/modules/ubf.js"
    "js/qlobber.min.js"
    "js/pubzub.js"

    "js/modules/z.live.js"
    "js/modules/z.notice.js"
    "js/modules/z.tooltip.js"
    "js/modules/z.dialog.js"
	"js/modules/livevalidation-1.3.js"
	"js/modules/jquery.loadmask.js"
%}

{% script %}

<script type="text/javascript">
	$(function()
	{
	    $.widgetManager();
	});
</script>
