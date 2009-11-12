<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js"></script>
<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jqueryui/1.7.2/jquery-ui.min.js"></script>

{% lib 
	"js/apps/zotonic-1.0.js"
	"js/apps/z.widgetmanager.js"
	"js/apps/modernizr.js"
	"js/cufon.anja.js"
%}

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