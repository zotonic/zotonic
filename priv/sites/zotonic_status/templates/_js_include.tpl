{% include "_js_include_jquery.tpl" %}
{% lib 
	"js/apps/zotonic-1.0.js"
	"js/apps/z.widgetmanager.js"
	"js/modules/livevalidation-1.3.js"
	"js/modules/z.inputoverlay.js"
	"js/cufon.anja.js"
%}

{% stream %}
{% script %}

<script type="text/javascript">
	$(function()
	{
	    $.widgetManager();
	});
	
	Cufon.registerFont(cufon_anja);
	Cufon.replace('h1',{
		color: '-linear-gradient(#19B7E8, #5bbde5)',
		hover: true,
		textShadow: '#222 1px 1px'
	});
	
	Cufon.replace('h2', {
		color: '-linear-gradient(#19B7E8, #5bbde5)',
		hover: true,
		textShadow: '#222 1px 1px'
	});
</script>
