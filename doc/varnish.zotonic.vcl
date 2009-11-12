# Varnish configuration for a Zotonic site
#
# It is a very simple configuration in which static content and the home page is cached
# and all page requests are handled by the Zotonic server (which does its own caching).
#
# On *nix:
#    sudo varnishd -a :80 -T localhost:6082 -f varnish.zotonic.vcl -s malloc -u nobody
#
# On Mac OS X you need to use the varnish.launchd.plist.
#    sudo cp varnish.zotonic.vcl /usr/local/etc/varnish/.
#    sudo cp varnish.launchd.plist /usr/local/etc/varnish/.
#    sudo chown root:wheel /usr/local/etc/varnish/*
#    sudo launchctl load /usr/local/etc/varnish/varnish.launchd.plist
#    sudo start launchd
#
# Or run varnishd in the foreground:
#    sudo varnishd -F -a :80 -T localhost:6082 -f varnish.zotonic.vcl -s malloc -u nobody
#
# You might want to change the req.http.host and the backend names.

backend zotonic {
  .host = "127.0.0.1";
  .port = "8000";
}


sub vcl_recv {
  set req.http.host = "zotonicsite.com";
  set req.backend = zotonic;

  # Add a unique header containing the client address
  unset req.http.X-Forwarded-For;
  set   req.http.X-Forwarded-For = client.ip;

  # We only deal with GET and HEAD by default
  if (req.request != "GET" && req.request != "HEAD") {
    return (pass);
  }

  # Cache the home page for a short period (ttl = 1 second, see vcl_fetch)
  if (req.url ~ "^/$") {
    unset req.http.Cookie;
    unset req.http.Authenticate;
    set req.grace = 10s;
    return (lookup);
  }

  # Cache served css and media files
  if (req.url ~ "^/(lib|image|media|favicon.ico)/") {
    unset req.http.Cookie;
    unset req.http.Authenticate;
    set req.grace = 30m;
    return (lookup);
  }

  return (pass);
}


sub vcl_pipe {
    # Note that only the first request to the backend will have
    # X-Forwarded-For set.  If you use X-Forwarded-For and want to
    # have it set for all requests, make sure to have:
    set req.http.connection = "close";
    return (pipe);
}


sub vcl_pass {
  if (req.url ~ "^/comet") {
    #bereq.connect_timeout = 70;
    #bereq.first_byte_timeout = 70;
  }
  return (pass);
}


sub vcl_fetch {
  if (req.url ~ "^/$") {
    unset obj.http.Set-Cookie;
	set obj.grace = 10s;
	set obj.ttl = 1s;
	return (deliver);
  }
  if (req.url ~ "^/(lib|image|media|favicon.ico)/") {
    unset obj.http.Set-Cookie;
    set obj.grace = 20m;
    set obj.ttl = 30m;
    return (deliver);
  }
  return (pass);
}


