{% if m.acl.user and (1 or m.session.facebook_logon) %}
<div id="fb-root"></div>
<script src="http://connect.facebook.net/en_US/all.js"></script>
<script>
  FB.init({appId: '{{ m.config.mod_facebook.app_id.value|default:"106094309435783" }}', status: true, cookie: true, xfbml: true});
  FB.Event.subscribe('auth.sessionChange', function(response) {
    if (response.session) {
      // A user has logged in, and a new cookie has been saved
    } else {
      // The user has logged out, and the cookie has been cleared
    }
  });
</script>
{% endif %}
