{# initialize the translation tabs #}
    <script type="text/javascript">
	/* Initialize translation tabs, select correct language */
    $(document).ready(function(){
	if ($(".translations").length) {
		$(".translations").tabs();
		$(".translations").bind('tabsshow', function(event, ui) {
			$(".tinymce-init", ui.panel).each(function() { 
				var self = $(this);
				setTimeout(function() { 
					var ti = jQuery.extend({}, tinyInit);
					if (self.attr('dir')) {
						ti.directionality = self.attr('dir');
					}
					self.tinymce(ti);
				}, 200);
			}).removeClass('tinymce-init').addClass('tinymce');
			$(".translations").tabs("select", ui.index);
		});

		var tab_index = $(".translations ul.ui-tabs-nav .tab-{{ z_language }}:visible").attr('data-index');
		if (typeof(tab_index) == 'undefined') {
			tab_index = $(".translations ul.ui-tabs-nav li:visible").attr('data-index');
		}
		if (typeof(tab_index) != "undefined") {
			$(".translations").tabs("select", parseInt(tab_index));
		}
	}
    });
    </script>
