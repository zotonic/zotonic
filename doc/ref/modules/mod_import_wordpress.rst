.. highlight:: django
.. include:: meta-mod_import_wordpress.rst

Import wordpress .wxr files in your site.

`Wordpress <http://wordpress.org/>`_, the famous PHP blog system, is
able to export all of its contents into a ``.wxr`` XML file. With this
module, you can import such a .wxr file in Zotonic, when you want to
migrate your blog. The import routine will import all posts, keywords,
categories, authors and images.

Once you enabled the module, the import button lives in the admin in
the "status" tab.

The import module works incrementally: if you import a .wxr file
twice, it will update the changed entries and add new entries. If you
delete posts, a next import will not re-create the entries: it will
remember that you have deleted them. You can override this behaviour
by checking the "import previously deleted content" button.
 
**Note:** YMMV with importing 2 .wxr files from different blogs: as
the unique keys of the entries are based on the wordpress numeric
id's, it is possible that content from the second one will overwrite
content from the previous import.


