{# initialize the translation tabs, select correct language #}
{% javascript %}
if ($(".translations").length) {
	$(".translations").on('show', 'ul li a', function(event) {
			const panel = $($(event.currentTarget).attr("href"));
			$(".tinymce-init", panel).each(function() {
				const self = $(this);
				setTimeout(function() {
					const ti = jQuery.extend({}, tinyInit);
					if (self.attr('dir')) {
						ti.directionality = self.attr('dir');
					}
					self.tinymce(ti);
				}, 200);
			}).removeClass('tinymce-init').addClass('tinymce');
	});
}
{% endjavascript %}
