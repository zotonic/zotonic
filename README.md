Welcome to Zotonic
==================

[![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=zotonic&url=https://github.com/zotonic/zotonic&title=zotonic&language=en_GB&tags=github&category=software) 

Here are some starting points for your journey with Zotonic.

INSTALLING
----------
When you want to install Zotonic, please read the file "doc/INSTALL".

When you want to proxy Zotonic using nginx, then check also the file
"doc/INSTALL.nginx" for an example site definition. Similarly,
doc/INSTALL.varnish shows how to put Zotonic behind the varnish http
frontend.


FOR ERLANG HACKERS
------------------
When you want to read the source, then start with
"src/zotonic_sup.erl" and work your way down using the servers defined
in there.  After you have an idea what is what.  Then check the
modules to see how they implement all the functionality, start with
"mod_base".


FOR TEMPLATE WIZARDS
--------------------
Sites live in priv/sites/*. Their templates are in organized in
subfolders. For the templates of the default site, look in
priv/sites/default/templates.

After that, start to scan the modules to see how templates, actions,
resources and css/javascript are defined.  Especially the module
"mod_base" is of interest as it defines all basic functionality which
other modules build upon.  From there continue with "mod_admin" and
see how it defines the admin interface, start with the webmachine
resources to see which template is used by which resource.


FOR SQL MAGICIANS
-----------------
Another useful source of information is the module
"src/install/z_install.erl", this erlang module contains the base SQL
datamodel.  Check also "src/install/z_install_data.erl" as it defines
a basic set of predicates, categories and whatnot.


Thank you and have fun with Zotonic.

Tim Benniks, Arjan Scherpenisse & Marc Worrell
