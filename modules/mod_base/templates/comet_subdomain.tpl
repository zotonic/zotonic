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
	function get_poll_count()
	{
		var old_domain = document.domain;
		document.domain = "{{ m.site.document_domain }}";
		var ws_pong_count = window.parent.z_ws_pong_count;
		try {
			document.domain = old_domain;
		} catch (e) {
		}
		return ws_pong_count;
	}

	function handle_poll_data(data)
	{
		var old_domain = document.domain;
		try {
			document.domain = "{{ m.site.document_domain }}";
			window.parent.z_comet_data(data);
		} catch (e) {
			if (window.console && window.console.log)
				window.console.log(e);
		}
		try {
			document.domain = old_domain;
		} catch (e) {
		}
	}

	function z_comet_poll_subdomain() 
	{
		if (get_poll_count() === 0) {
			$.ajax({ 
				url: '/comet',
				type: 'post',
				data: "z_pageid={{ q.z_pageid|urlencode }}",
				dataType: 'text',
				success: function(data, textStatus) 
				{
					handle_poll_data(data);
					setTimeout(function() { z_comet_poll_subdomain(); }, 500);
				},
				error: function(xmlHttpRequest, textStatus, errorThrown) 
				{
					setTimeout(function() { z_comet_poll_subdomain(); }, 1000);
				}
			});
		} else {
			setTimeout(function() { z_comet_poll_subdomain(); }, 5000);
		}
	}
	z_comet_poll_subdomain();
	</script>
  </body>
</html>
