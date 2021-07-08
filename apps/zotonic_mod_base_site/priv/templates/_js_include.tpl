
{% include "_js_include_jquery.tpl" %}

{% lib
    "js/modules/jstz.min.js"
    "cotonic/cotonic.js"
	"js/apps/zotonic-wired.js"
	"js/apps/z.widgetmanager.js"
	"js/modules/z.notice.js"
	"js/modules/z.imageviewer.js"
	"js/modules/z.dialog.js"
	"js/modules/z.clickable.js"
	"js/modules/livevalidation-1.3.js"
	"js/modules/jquery.loadmask.js"
	"bootstrap/js/bootstrap.min.js"
	"js/modules/responsive.js"
%}

{% block _js_include_extra %}{% endblock %}

<script type="text/javascript">
	$(function()
	{
	    $.widgetManager();
	});
</script>

{% worker name="auth" src="js/zotonic.auth.worker.js" args=%{  auth: m.authentication.status  } %}

