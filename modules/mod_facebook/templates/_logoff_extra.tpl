
<p>Logging out of Facebook…</p>
	
<div id="fb-root"></div>
<script src="http://connect.facebook.net/en_US/all.js"></script>
<script>
  FB.init({appId: '{{ m.config.mod_facebook.app_id.value|default:"106094309435783" }}', status: true, cookie: true, xfbml: true});
  FB.Event.subscribe('auth.sessionChange', function(response) {
    if (response.session) {
      // The user was logged in, and a new cookie has been saved
	   FB.logout(function() { window.location = '/';})
    } else {
      // The user has logged out, and the cookie has been cleared
       window.location = '/';
    }
  });
</script>
