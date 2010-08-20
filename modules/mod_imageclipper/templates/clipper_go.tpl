<body>
    <h1>One moment please...</h1>

	<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"></script>
	<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jqueryui/1.7.2/jquery-ui.min.js"></script>

	{% lib 
		"js/apps/zotonic-1.0.js"
        "js/modules/jquery.loadmask.js"
	%}
	
	<script type="text/javascript">
		$(function() 
		{ 
        console.log(1);
		});
	</script>

	{# Initialize the zotonic postback #}
	{% stream %}
	{% script %}

</body>

