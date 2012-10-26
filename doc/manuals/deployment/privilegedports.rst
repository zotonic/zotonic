.. _manual-deployment-privilegedports:

Running on Port 80 and Port 443
===============================

Using standard ports helps visitors discover your page and removes the
awkward port number from URLs.

The HTTP and HTTPS protocols are normally served on TCP ports 80
and 443. It is beneficial to run a these services on those standard
ports: it aids discovery and lends a air of polish while a
non-standard port number suggests something is incomplete.

\*nix systems only allow the superuser (root) to bind to ports
below 1024. HTTP & HTTPS use ports 80 & 443 respectively. So setting
up Zotonic to serve from those ports without running as superuser
presents a problem, as \*nix considers all ports below 1024 to be
"privileged", requiring special access.

This manual outlines three different methods to let Zotonic listen on
port 80. All of them are for \*nix based systems only.


Pre-launch notes
----------------

Before Zotonic serves its sites on a privileged ports, the hostname
portions of your Zotonic sites need to be changed to reflect this.

For production release of your new Zotonic site you need to:

- Make sure that your server has a public IP address, and that the
  server accepts connections on port 80.

- For each of your Zotonic sites, configure their DNS
  (e.g. `www.mysite.com`) to point to your server's IP address.

- Change ``{hostname, "mysite:8000"}`` to ``{hostname,
  "www.mysite.com"}`` in ``priv/sites/mysite/config``.  This last
  change enables the virtual hosting: it makes sure that Zotonic knows
  which site is being requested when somebody visits `www.mysite.com`.


Running behind another web server / proxy
-----------------------------------------

You run another web server to proxy requests from port 80 to 8000 and
from 443 to 8443.  :ref:`Varnish <manual-deployment-varnish>` and
:ref:`nginx <manual-deployment-nginx>` are both very capable web
servers for doing this.

However, Zotonic itself is also very capable of directly serving web
content without needing an extra caching layer in between. The other
methods listed below explain how Zotonic can obtain direct access to
the privileged ports.


Using authbind
--------------

Note: Instructions below assume site is named ``mysite`` and Zotonic is
installed in ``/home/zotonic/zotonic``. Replace as appropriate.

Install authbind::

  zotonic:~$ sudo apt-get install authbind 

Configure authbind to allow zotonic user to access port 80::

  zotonic:~$ sudo touch /etc/authbind/byport/80
  zotonic:~$ sudo chown zotonic /etc/authbind/byport/80
  zotonic:~$ sudo chmod 500 /etc/authbind/byport/80

Set up the environment.

You need to tell Zotonic explicitly which IP address to listen
on. Zotonic defaults to ``any``, which will also bind to any ipv6
addresses available. However authbind doesn't work with ipv6 and so
will cause Zotonic to crash on startup.

Add the following entries to ``/home/zotonic/.profile``, then save file & exit::

  export ZOTONIC_PORT=80
  export ZOTONIC_PORT_SSL=443
  public_interface=eth0
  export ZOTONIC_IP=`/sbin/ifconfig $public_interface | grep 'inet addr:' | cut -d: -f2 | awk '{ print $1}'`
  export ERL="authbind --deep erl"

Where ``eth0`` is the name of the Ethernet interface that connects to the
Internet. (For a MediaTemple (ve) host this was venet0:0) You could
alternatively set it to ``lo`` for localhost-only testing or to a LAN-only
interface (say eth1) for a multi-interface server you are using
Zotonic to host an intranet site with.

Source the file to update the environment::

  zotonic:~$ . ~/.profile

Delete the Zotonic config file (this will be re-generated automatically when zotonic next starts up)::

  zotonic:~$ rm ~zotonic/zotonic/priv/config
  
Set the port for your site. Edit the hostname entry in ~zotonic/priv/sites/yoursite/config to read as follows::

  {hostname, "yoursite:80"}

Stop zotonic if already running::

  zotonic:~$ ~/zotonic/bin/zotonic stop

Start zotonic::

  zotonic:~$ ~/zotonic/bin/zotonic start

Browse to http://yoursite/ and verify that everything is working like it should.


Using setcap
------------

Warning: this is a much broader approach as it grants privileged bind
to all Erlang VM processes (the ``beam`` and ``beam.smp``
executables).  Unless you are the sole user of such a machine this is
not a great idea.

From a shell, install the setcap program::

  sudo apt-get install libcap2-bin 

Now configure setcap to allow Erlang BEAM processes user to bind to ports lower than 1024::

  sudo setcap 'cap_net_bind_service=+ep' /usr/lib/erlang/erts-5.9.2/bin/beam
  sudo setcap 'cap_net_bind_service=+ep' /usr/lib/erlang/erts-5.9.2/bin/beam.smp

Note that the exact paths to the ``beam`` and ``beam.smp`` can be
different, depending on the Erlang version.
  
During package upgrades Erlang may be upgraded and your site will seem
to be broken. Just make sure to check the ERTS version and rerun these
setcaps commands for the new version.

For more granular control, you could create an Erlang release that
only the Zotonic User can access.  Once the release is created ``setcap``
could be applied to the beam and beam.smp within that release only.


Using iptables
--------------

If authbind and setcap will not work for you, using the system
firewall to redirect the ports can be an option.

Firewall prerouting can be enabled as follows to forward communication
on port 80 to port 8000 and port 443 to port 8443::

  iptables -t nat -A PREROUTING -p tcp --dport 80 -j REDIRECT --to 8000
  iptables -t nat -A PREROUTING -p tcp --dport 443 -j REDIRECT --to 8443

You also need two more rules so that the site can reach itself. In the
following firewall rules, replace ``your.ip.address`` with your
external IP address::
  
  iptables -t nat -A OUTPUT -p tcp -d your.ip.address --dport 80 -j REDIRECT --to 8000
  iptables -t nat -A OUTPUT -p tcp -d your.ip.address --dport 443 -j REDIRECT --to 8443

The downside of using the firewall is that Zotonic still also listens
on port 8000. This might be a cause for confusion.

For instructions on how to save these firewall rules and reinstate
them after a system reboot, consult the `Ubuntu firewall
administration manual
<https://help.ubuntu.com/community/IptablesHowTo#Configuration_on_startup>`_.

