.. _manual-deployment-nginx:

Proxying Zotonic with nginx
===========================

It is possible to put Zotonic behind the `nginx <http://nginx.org/>`
web server, for example if you have other, non-Zotonic virtual hosts
running on your system.

When proxying, don't forget to check the config files of the sites you
are planning to server (the ``priv/sites/your_site/config``
files). The ``hostname`` value should not contain any port number, if
you run from port 80: ``{hostname, "test.zotonic.com"}``.

Below is a configuration file we use to proxy nginx to zotonic. Be
sure to replace all occurrences of `test.zotonic.com` with your own
hostname::

  server {
        listen 80;
        server_name  test.zotonic.com;

        access_log  /var/log/nginx/test.zotonic.com.access.log;
        error_log  /var/log/nginx/test.zotonic.com.error.log;

        keepalive_timeout 65;
        gzip off;

        location / {
            proxy_pass http://127.0.0.1:8000/;
            proxy_redirect off;
            
            proxy_set_header   Host             $host;
            proxy_set_header   X-Real-IP        $remote_addr;
            proxy_set_header   X-Forwarded-For  $proxy_add_x_forwarded_for;

            client_max_body_size       50m;
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

See the `nginx documentation <http://nginx.org/en/docs/>`_ for more
information on its configuration procedure.
