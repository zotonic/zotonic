{% javascript %}
$('#rscform').on('shown.bs.tab', '.language-tabs > li > a[data-toggle="tab"]', function (e) {
	if (e.target != e.relatedTarget) {
		var lang = $(e.target).parent().attr('lang');
		$("li[lang='"+lang+"']:visible > a").tab('show');
		z_editor.init();
	}
});

cotonic.broker.subscribe("bridge/origin/model/rsc/event/{{ id }}/delete", function(msg) {
	if ($('#rscform input[name=id]').val() == '{{ id }}') {
		$('#rscform').replaceWith("<p class='alert alert-warning'><b>{_ Deleted. _}</b> {_ This page has been deleted. _}</p>");
	}
});
{% endjavascript %}
