.. _guide-deployment-nginx:

Proxying Zotonic with nginx
===========================

It is possible to put Zotonic behind the `nginx <http://nginx.org/>`
web server, for example if you have other, non-Zotonic virtual hosts
running on your system.

When proxying, don't forget to check the config files of the sites you
are planning to server (the ``user/sites/your_site/config``
files). The ``hostname`` value should not contain any port number, if
you run from port 80/443: ``{hostname, "test.zotonic.com"}``.

Zotonic configuration
---------------------

Example of ``~/.zotonic/[release]/zotonic.config`` ip/port settings when
terminating SSL in nginx proxy and using plain HTTP towards the Zotonic
backend:

.. code-block:: erlang

  %%% IP address on which Zotonic will listen for HTTP requests.
  {listen_ip, any},

  %%% Port on which Zotonic will listen for HTTP requests.
  {listen_port, 8000},

  %%% Port on which Zotonic will listen for HTTPS requests.
  %%% Set to the atom 'none' to disable SSL
  {ssl_listen_port, none},

  %%% Outside port on which Zotonic will listen for HTTP requests.
  {port, 80},

  %%% Outside port zotonic uses to receive incoming HTTPS requests.
  {ssl_port, 443},

Nginx configuration
-------------------

Below is an example configuration file to proxy nginx to zotonic. Be
sure to replace all occurrences of ``test.zotonic.com`` with your own
hostname:

.. code-block:: nginx

  server {
        listen 80;
        listen   [::]:80 default_server ipv6only=on; ## listen for ipv6

        listen 443 ssl http2;
        listen [::]:443 ssl http2 ipv6only=on;

        server_name  test.zotonic.com;

        access_log  /var/log/nginx/test.zotonic.com.access.log;
        error_log  /var/log/nginx/test.zotonic.com.error.log;

        keepalive_timeout 65;
        gzip off;

        ssl_protocols TLSv1.2;
        ssl_prefer_server_ciphers on;
        ssl_ciphers 'ECDHE-RSA-AES256-GCM-SHA384:ECDHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-SHA384:ECDHE-RSA-AES128-SHA256:ECDHE-RSA-AES256-SHA:ECDHE-RSA-AES128-SHA:DHE-RSA-AES256-SHA256:DHE-RSA-AES128-SHA256:DHE-RSA-AES256-SHA:DHE-RSA-AES128-SHA:AES256-GCM-SHA384:AES128-GCM-SHA256:AES256-SHA256:AES128-SHA256:AES256-SHA:AES128-SHA:HIGH:!aNULL:!eNULL:!EXPORT:!DES:!MD5:!PSK:!RC4';

        # Disable preloading HSTS for now. Enable when you know that your
        # server certs works. You can use the header line that includes
        # the "preload" directive if you understand the implications.
        # add_header Strict-Transport-Security "max-age=15768000; includeSubdomains"; # six months
        # add_header Strict-Transport-Security "max-age=15768000; includeSubdomains; preload";

        ssl_session_cache shared:SSL:10m;
        ssl_session_timeout 5m;

        # create with: openssl dhparam -out /etc/nginx/dhparam.pem 2048
        # ssl_dhparam /etc/nginx/dhparam.pem;

        ssl_certificate /path/to/ssl.crt;
        ssl_certificate_key /path/to/ssl.key;

        location / {
            proxy_pass http://127.0.0.1:8000/;
            proxy_redirect off;

            proxy_set_header  Host              $http_host;
            proxy_set_header  X-Real-IP         $remote_addr;
            proxy_set_header  X-Forwarded-For   $proxy_add_x_forwarded_for;
            proxy_set_header  X-Forwarded-Proto $scheme;
            proxy_pass_request_headers on;


            client_max_body_size       0;
            client_body_buffer_size    128k;

            proxy_connect_timeout      90;
            proxy_send_timeout         90;
            proxy_read_timeout         90;

            proxy_buffer_size          4k;
            proxy_buffers              4 32k;
            proxy_busy_buffers_size    64k;
            proxy_temp_file_write_size 64k;
        }

        location /close-connection {
             keepalive_timeout 0;
             empty_gif;
        }
  }

Remember to add X-Forwarded-Proto to proxied header so that Zotonic
knows that HTTPS is used before the proxy even though HTTP is used between
the proxy and backend. Also add X-Real-IP and X-Forwarded-For headers.

Zotonic always redirects to HTTPS so the proxy needs to be configured for
both HTTP and HTTPS.

Zotonic makes use of a websocket connection for MQTT messages at the
``/mqtt-transport`` endpoint, so you need to pass the ``Upgrade``
and ``Connection`` headers.

The ``/mqtt-transport`` endpoint is also used to POST uploaded files
using a HTML multi-part form post.

See the `nginx documentation <http://nginx.org/en/docs/>`_ for more
information on its configuration procedure.
