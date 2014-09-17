{% javascript %}
setTimeout(function() {
	$({{ m.session['admin_widgets']|to_json }}).each(function() {
		for (var k in this) {
			$("#"+k).adminwidget("setVisible", this[k] == "true", true);
		}});
	}, 1);
	
$('#rscform').on('shown', '.language-tabs > li > a[data-toggle="tab"]', function (e) {
	if (e.target != e.relatedTarget) {
		var lang = $(e.target).parent().attr('lang');
		$("li[lang='"+lang+"']:visible > a").tab('show');
		z_editor.init();
	}
});
{% endjavascript %}
