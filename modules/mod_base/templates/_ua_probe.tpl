{# Javascript that probes the user-agent and performs a callback with the results. #}
{% with m.req.ua_props as ua_props %}
<script type="text/javascript">
(function() {
	var ua_curr = {w: window.screen.width, h: window.screen.height, t: (('ontouchstart' in window)?"1":"0")};
	if(({{ ua_props.displayWidth|default:0 }} == ua_curr.w) 
		&& ({{ ua_props.displayHeight|default:0 }} == ua_curr.h) && ({{ ua_props.is_touch|if:1:0 }} == ua_curr.t)) 
		return;
	z_post("/useragent/probe", ua_curr);
})();
</script>
{% endwith %}
