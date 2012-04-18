{# initialize the translation tabs, select correct language #}
{% javascript %}
if ($(".translations").length) {
	$(".translations ul li a").live('show', function(event) {
			var panel = $($(event.currentTarget).attr("href"));
			$(".tinymce-init", panel).each(function() { 
				var self = $(this);
				setTimeout(function() { 
					var ti = jQuery.extend({}, tinyInit);
					if (self.attr('dir')) {
						ti.directionality = self.attr('dir');
					}
					self.tinymce(ti);
				}, 200);
			}).removeClass('tinymce-init').addClass('tinymce');
	});
}
{% endjavascript %}
