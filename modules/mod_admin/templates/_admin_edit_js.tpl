{% javascript %}
setTimeout(function() {
	$({{ m.session['admin_widgets']|to_json }}).each(function() {
	for (var k in this) {
			$("#"+k).adminwidget("setVisible", this[k], true);
		}});
	}, 1);

    $('#rscform').on('shown.bs.tab', '.language-tabs > li > a[data-toggle="tab"]', function (e) {
	if (e.target != e.relatedTarget) {
		var lang = $(e.target).parent().attr('lang');
		$("li[lang='"+lang+"']:visible > a").tab('show');
		z_editor.init();
	}
});

pubzub.subscribe("~site/rsc/{{ id }}", function(topic, msg) {
	if (msg.payload._record == 'rsc_update_done') {
		if (msg.payload.action == 'delete' && $('#rscform input[name=id]').val() == {{ id }}) {
			$('#rscform').replaceWith("<p class='alert alert-warning'><b>{_ Deleted. _}</b> {_ This page has been deleted. _}</p>");
		}
	}
});
{% endjavascript %}
