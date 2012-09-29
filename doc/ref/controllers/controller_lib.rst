
.. include:: meta-lib.rst

Serve css, javascript and simple images.

This controller is used to serve files and images, which are located
under the the ``lib/`` folder of modules and sites.

Multiple files can be served as one file; the controller can combine them in one single
request. See the :ref:`tag-lib` tag for more information.  Javascript files
are concatenated with a semi-colon and newline, all other files are
concatenated with a newline character between the files.

This resource serves all files with a very long client side caching time. It also handles if-modifies-since checks. And it compresses served files with gzip when the user-agent supports it.

When serving multiple files then the modified response header will be set to the modification date of the newest file.

Example dispatch rule:

{lib, ["lib", '*'], resource_lib, []}

Resource_file_readonly has the following dispatch options:
Option	Description	Example
root	List of root directories where files are located. Use 'lib' for the library files. This defaults to the lib directory.	{root, ["/var/www/css/"]}
content_disposition	If the file should be viewed in the browser or downloaded. Possible values are inline and attachment. Defaults to the browser’s defaults by not setting the “Content-Disposition” response header.	{content_disposition, inline}
use_cache	Use server side caching of files. Especially useful when gzip-compressing files or serving many combined css or javascript files. Less useful when a proxy cache like Varnish is used. Defaults to false.	{use_cache, true}

Resource_lib does not handles any query arguments other than the file path.
