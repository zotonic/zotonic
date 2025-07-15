{% javascript %}
$('#rscform, #translation-tabs').on('shown.bs.tab', '.language-tabs > li > a[data-toggle="tab"]', function (e) {
	if (e.target != e.relatedTarget) {
        const showLang = e.target.closest('li').getAttribute('lang');
        const hideLang = e.relatedTarget.closest('li').getAttribute('lang');

		$("li[lang='"+showLang+"'] > a").tab('show');

		// Also switch language dependent parts that are not inside the tab panes.
		$(".widget-content-lang-" + hideLang).hide()
		$(".widget-content-lang-" + showLang).show();

		z_postback_data_set('z_edit_language', showLang);

		setTimeout( () => z_editor.init(), 1 );
	}
});

const activeLang = $('#translation-tabs .active');
if (activeLang.length) {
	const lang = activeLang.attr('lang');
	z_postback_data_set('z_edit_language', lang);
}

cotonic.broker.subscribe("bridge/origin/model/rsc/event/{{ id }}/delete", function(msg) {
	if ($('#rscform input[name=id]').val() == '{{ id }}') {
		$('#rscform').replaceWith("<p class='alert alert-warning'><b>{_ Deleted. _}</b> {_ This page has been deleted. _}</p>");
	}
});
{% endjavascript %}
