{# Javascript that probes the user-agent and performs a callback with the results. #}
<script type="text/javascript">
{% if not m.acl.user.pref_tz and not m.config.site.timezone_is_fixed.value %}
if (typeof(jstz) == "object") {
    var ua_tz = jstz.determine();
    if (ua_tz.name() != '{{ m.req.timezone }}') {
        var ua_probe = "tz=" + urlencode(ua_tz.name());
        document.write(unescape("%3Cscript src='/useragent/probe.js?"+ua_probe+"' type='text/javascript'%3E%3C/script%3E"));
    }
}
{% endif %}
</script>
