.. _tutorial-install-addsite:

Adding your first site
======================

Before adding your first site, make sure you got Zotonic running and
are looking at the nice `Powered by Zotonic` web page, as described in
:ref:`tutorial-install`.

Now that you got Zotonic running, it is time to create your first
website with it. In this example, the site we are going to add is
called ``yoursite``.

1. Edit your ``/etc/hosts`` file, adding an entry for ``yoursite`` to point at your local host::

     yoursite   127.0.0.1
     
2. Create an user and database in PostgreSQL (change the password for the user!)::

     CREATE USER zotonic WITH PASSWORD 'zotonic';
     CREATE DATABASE zotonic WITH OWNER = zotonic ENCODING = 'UTF8';
     GRANT ALL ON DATABASE zotonic TO zotonic;
     \c zotonic
     CREATE LANGUAGE "plpgsql";

3. Create a new zotonic site, based on the "blog" skeleton site::

     bin/zotonic addsite -s blog yoursite

   This will add a site named yoursite. Its default URL will be
   http://yoursite:8000/ so either put 'yoursite' in your hosts file
   or change the {hostname} section of the config file.

4. Edit the generated file priv/sites/yoursite/config, to make sure
   your database credentials and the hostname are correct, and change
   the password for the admin.

5. Rebuild Zotonic by typing ``make``, and then start it using ``bin/zotonic debug``.

6. Now Zotonic will install the initial database. When something goes
   wrong here, then it is almost always a problem with the database
   connection. Double-check your database configuration in the
   `priv/sites/yoursite/config` file.

7. Visit http://yoursite:8000/ in your browser to see your new blog,
   or go directly to http://yoursite:8000/admin/ to visit the admin
   section.

8. Congratulations! You got your first site running.

To learn more about how sites work and what consists of a
:term:`Zotonic site`, please see the next chapter,
:ref:`tutorial-site-anatomy`.
