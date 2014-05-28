.. _manual-cli-addsite:

The `zotonic addsite` command
=============================

Adding a site to Zotonic is done through the :ref:`zotonic <manual-cli>` shell command. It syntax is like this::
  
  zotonic addsite [options] <site_name>

This command creates a new site with [site_name] as the site's
name. This new site will be based on a so-called `skeleton
site`. Currently there are four skeletons: 'blog', 'basesite', 'empty'
and 'nodb'. 'blog' is the default.
  
The addsite command is highly configurable and takes the following options:

  -s <skel>    Skeleton site (one of 'blog', 'basesite', 'empty', 'nodb'; default: blog)
  -H <host>    Site's hostname (default: <site_name>.dev)
  -L           Create the site in the current directory and symlink it into the user sites directory
  -g <remote>  Create a git repository in the site and push it to the given remote

  -h <host>    Database host (default: localhost)
  -p <port>    Database port (default: 5432)
  -u <user>    Database user (default: zotonic)
  -P <pass>    Database password (default: zotonic)
  -d <name>    Database name (default: zotonic)
  -n <schema>  Database schema (default: public)
  -a <pass>    Admin password (default: admin)


Adding a site
-------------

When adding a site, the site will be created in the Zotonic user sites
directory. When ``-L`` is used, the site will be created in the
current directory (from which the ``addsite`` command is ran), and a
symlink into the Zotonic user sites directory will be made.

Before adding the site, the command will print out an
overview of what it will do before continuing.

For instance, consider the following addsite command::

  zotonic addsite -s blog myfirstblog

Will print out the following::

  ************
  Warning!
  ************
  Site: 'myfirstblog.dev' cannot be reached.
  Command 'host myfirstblog.dev' must resolve to an IP address,
  otherwise you won't be able to reach it after installing the site.
  You can fix that by adding the following line to /etc/hosts:
  
  127.0.0.1         myfirstblog.dev
  
  
  ==== Add site ====
  
  Site name: myfirstblog
  Site URL: http://myfirstblog.dev:8000/
  
  Skeleton site: blog
  Site directory: /home/user/zotonic/user/sites/myfirstblog
  Admin password: admin
  
  Database host: 127.0.0.1
  Database port: 5432
  Database user: zotonic
  Database password: zotonic
  Database name: zotonic
  Database schema: public
  
  >>> Hit return to proceed...


First, it will warn you that the hostname that this site (initially)
will have, is not yet resolvable. Add the hostname to your local hosts
file to be able to see the site when it has finished installing.

After this warning, the `addsite` command will print out an overview
of what it will do. It will show the site name, the URL the site will
be reachable on, in which directory the site will be installed, et
cetera. An overview of the database credentials and the admin password
will also be printed. After hitting return, the site will be created
and built for the first time. This will take a few moments, after
which you will be able to visit the site's URL in your browser.
  
  
Default values to `zotonic addsite`
-----------------------------------

The "addsite" subcommand checks a file called
``$HOME/.zotonic-defaults`` for the default values to these
options. This file is a file in bash-syntax which can define the
following variables: ``SKEL``, ``DBHOST``, ``DBPORT``, ``DBUSER``,
``DBPASSWORD``, ``DBDATABASE``, ``DBSCHEMA``, ``ADMINPASSWORD``,
``SITEHOSTNAME``, ``DO_LINK``, ``TARGETDIR``.

For instance, if you want all new Zotonic sites to be created in
/var/www and have hostnames like `www.mysite.intra`,
`www.anothersite.intra`, add the following to your
``$HOME/.zotonic-defaults`` file::

  export SITEHOSTNAME="www.%%SITE%%.intra"
  export TARGETDIR=/var/www
    
