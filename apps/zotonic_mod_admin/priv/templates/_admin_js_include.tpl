{% include "_js_include_jquery.tpl" %}

{% lib
    "js/bootstrap3/bootstrap.min.js"
%}

{% lib
    "js/modules/jstz.min.js"

    "cotonic/zotonic-wired-bundle.js"

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
    "js/modules/z.menuedit.js"
    "js/modules/z.cropcenter.js"
    "js/modules/z.popupwindow.js"
    "js/modules/z.zeditor.js"
    "js/modules/livevalidation-1.3.js"
    "js/modules/jquery.loadmask.js"
    "js/modules/jquery.timepicker.min.js"

    minify
%}

{% all include "_admin_lib_js.tpl" %}

{% worker name="auth" src="js/zotonic.auth.worker.js" %}

<script type="text/javascript">
$(function()
{
	$.widgetManager();
});
</script>

