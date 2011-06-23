<html>
  <head>	
	<meta name="robots" content="noindex, nofollow" />
	<title>Comet subdomain handler of: {{ m.site.title }} </title>
  </head>

  <body>
	{#
		See: http://fettig.net/weblog/2005/11/28/how-to-make-xmlhttprequest-connections-to-another-server-in-your-domain/
		http://fettig.net/playground/ajax-subdomain/test5-iframe.html
	#}
	{% include "_js_include_jquery.tpl" %}

    <script type="text/javascript">
	function z_comet() 
	{
		$.ajax({ 
			url: '/comet',
			type: 'post',
			data: "z_pageid={{ q.z_pageid|urlencode }}",
			dataType: 'text',
			success: function(data, textStatus) 
			{
				var old_domain = document.domain;
				try 
				{
					document.domain = "{{ m.site.document_domain }}";
					window.parent.z_comet_data(data);
				} 
				catch (e)
				{
					if (window.console && window.console.log)
						window.console.log(e);
				}
				
				try {
					document.domain = old_domain;
				} catch (e) {
				};
				setTimeout("z_comet();", 200);
			},
			error: function(xmlHttpRequest, textStatus, errorThrown) 
			{
				setTimeout("z_comet();", 1000);
			}
		});
		return;
	}
	
	setTimeout("z_comet()", 1000);
	</script>
  </body>
</html>
