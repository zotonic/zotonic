.. _installation-install-addsite:

Adding your first site
======================

Before adding your first site, make sure you got Zotonic running and
are looking at the nice `Powered by Zotonic` web page, as described in
:ref:`installation-install`.

Now that you got Zotonic running, it is time to create your first
website with it. In this example, the site we are going to add is
called ``yoursite``.

1. Edit your ``/etc/hosts`` file, adding an entry for ``yoursite`` to point at your local host::

     127.0.0.1   yoursite
     
2. Create an user and database in PostgreSQL (change the password for the user!)::

     CREATE USER zotonic WITH PASSWORD 'zotonic';
     CREATE DATABASE zotonic WITH OWNER = zotonic ENCODING = 'UTF8';
     GRANT ALL ON DATABASE zotonic TO zotonic;
     \c zotonic
     CREATE LANGUAGE "plpgsql";

3. Create a new zotonic site, based on the "blog" skeleton site::

     bin/zotonic addsite -s blog yoursite

   .. note:: Zotonic has to be running for the `addsite` command to succeed.

   This will add a site named `yoursite`. Its default URL will be
   http://yoursite:8000/ so either put 'yoursite' in your hosts file as stated above,
   or change the ``{hostname}`` section of the site’s config file.

   Look at :ref:`skeleton-sites` to see which other skeletons are
   available to base your site on.

4. Edit the generated file ``priv/sites/yoursite/config``, to make sure
   your database credentials and the hostname are correct, and change
   the password for the admin.

5. Rebuild Zotonic by typing ``make``, and then (re)start it using ``bin/zotonic debug``.

6. Now Zotonic will install the initial database. If something goes
   wrong here, then it is almost always a problem with the database
   connection. Double-check your database configuration in the
   `priv/sites/yoursite/config` file.

7. Visit http://yoursite:8000/ in your browser to see your new blog,
   or go directly to http://yoursite:8000/admin/ to visit the admin
   section.

8. Congratulations! You got your first site running.

To learn more about how sites work and what consists of a
:term:`Zotonic site`, please see the in-depth manual,
:ref:`manual-site-anatomy`.


.. _skeleton-sites:
     
Available skeleton sites
------------------------

Zotonic comes with four different skeletons to base your site on.


``blog``
  As a full example of a Zotonic website, it installs a front page
  with a listing of recent articles. As default example data, three
  example articles and a couple of images are also installed.

``basesite``
  A skeleton site which lets you build a site on top of
  :ref:`mod_base_site`. Its site directory is pretty empty, as
  `mod_base_site` itself implements most of the frontend templates
  that are needed. This skeleton does install a custom homepage
  template as ``home.tpl`` and dispatch rule to serve it. It also adds
  a `site.css` file for tweaking fonts, colors, et cetera.

``empty``
  An empty skeleton. No templates or dispatch rules whatsoever are
  created. You can use this skeleton to create a new site based on
  your own base templates, a custom CSS framework, etc.

``nodb``
  Like the `empty` template, but this skeleton does not require a
  database connection. As such, the admin and content management
  interface is disabled, as those modules all require a database
  connection.
  
