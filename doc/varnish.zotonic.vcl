#
# Varnish configuration for a Zotonic site
#

backend zotonic_com {
  .host = "127.0.0.1";
  .port = "8000";
  .first_byte_timeout = 300s;
  .connect_timeout = 300s;
  .between_bytes_timeout = 300s;
}

backend nginx {
  .host = "127.0.0.1";
  .port = "8080";
  .first_byte_timeout = 300s;
  .connect_timeout = 300s;
  .between_bytes_timeout = 300s;
}


sub vcl_recv {
  set req.http.X-Forwarded-Host = req.http.host;

  ######################################################################
  ########################### VIRTUAL HOSTS ############################

  if (req.http.host ~ "^(www.|)(zotonic|example).(com|net)$") {
    set req.backend   = zotonic_com;
  }
  # Nginx hosted sites
  #
  else {
    set req.backend   = nginx;
  }

  ######################################################################

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
  if (req.url ~ "^/(lib|image|media|favicon.ico)/") {
    unset beresp.http.Set-Cookie;
    set beresp.grace = 30m;
    set beresp.ttl = 10m;
    return (deliver);
  }
  return (pass);
}

sub vcl_error {
  if (obj.status == 503 && req.restarts < 4) {
    restart;
  }
}


