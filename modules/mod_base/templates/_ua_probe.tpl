{# Javascript that probes the user-agent and performs a callback with the results. #}
<script type="text/javascript">
var ua_probe = "w="+window.screen.width
			 + "&h="+window.screen.height
			 + "&t="+(('ontouchstart' in window)?"1":"0");
var ua_curr =  "w="+{{ m.req.ua_props.displayWidth|default:0 }}
			 + "&h="+{{ m.req.ua_props.displayHeight|default:0 }}
			 + "&t="+{{ m.req.ua_props.is_touch|if:1:0 }};
if (ua_probe != ua_curr) {
	document.write(unescape("%3Cscript src='/useragent/probe.js?"+ua_probe+"' type='text/javascript'%3E%3C/script%3E"));
}
</script>
