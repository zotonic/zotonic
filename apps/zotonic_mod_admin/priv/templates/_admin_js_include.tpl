{% include "_js_include_jquery.tpl" %}

{# {% lib
    "js/bootstrap3/bootstrap.min.js"
%} #}

{% lib
    "js/bootstrap5/bootstrap.bundle.min.js"
%}

{% lib
    "js/modules/jstz.min.js"

    "cotonic/cotonic.js"

    "js/apps/zotonic-wired.js"
    "js/apps/z.widgetmanager.js"

    "js/modules/jquery.hotkeys.js"

    "js/apps/admin-common.js"

    "js/jquery.ui.nestedSortable.js"

    "js/modules/z.adminwidget.js"

    "js/modules/z.live.js"
    "js/modules/z.notice.js"
    "js/modules/z.tooltip.js"
    "js/modules/z.dialog.js"
    "js/modules/z.feedback.js"
    "js/modules/z.formreplace.js"
    "js/modules/z.datepicker.js"
    "js/modules/z.listfilter.js"
    "js/modules/z.menuedit.js"
    "js/modules/z.popupwindow.js"
    "js/modules/z.zeditor.js"
    "js/modules/livevalidation-1.3.js"
    "js/modules/jquery.loadmask.js"
    "js/modules/jquery.timepicker.min.js"

    "js/prism.js"
    "js/zotonic-search-view.js"

    minify
%}

{% all include "_admin_lib_js.tpl" %}

{% worker name="auth" src="js/zotonic.auth.worker.js" args=%{  auth: m.authentication.status  } %}

<script type="text/javascript" nonce="{{ m.req.csp_nonce }}">
$(function()
{
	$.widgetManager();
});
</script>
