
.. include:: meta-mod_artwork.rst

This module contains many useful icons and images.

Included icons and images
-------------------------

The following icons and images are included:

``lib/images/emotes/``
	A collection of emoticons. These are from the Tango Icon Library and in the Public Domain.

``lib/images/flags/``
	A collection of country flags. They are coded using the two letter ISO-3166 code of the country. 
	For example :file:`lib/images/flags/flag-nl.png` refers to the Dutch flag.
	This collection is from Wikimedia Commons and in the Public Domain. The PNG files are created by the
	Open Icon Library.

``lib/images/logo/``
	Some Zotonic logos, with space for more.

``lib/images/mimeicons/``
	Zotonic can’t generate a preview of all files. If it can’t generate a preview then an icon
	representing the mime type of the file is used. This is a collection of images for some mime types.
	This collection is from the Tango Icon Library and in the Public Domain.

``lib/images/noun/``
	A selection of icons from The Noun Project. There are many black&white icons in this directory.
	This collection is licensed with the Creative Commons Attribution 3.0 Unported (CC BY 3.0) License, though
	about half of the icons are in the Public Domain or licensed using CC 0.
	When you use an icon in your project, check the license on http://thenounproject.com/ and the proper 
	attribution as described in http://thenounproject.com/using-symbols/

``lib/images/social/round/``
	Round icons for various social sites. Think of Youtube, Twitter, Facebook etc. This collection is 
	made by Veodesign (http://veodesign.com/) 
	It is licensed under the Creative Commons Attribution Share-Alike 3.0 Unported License.
	This means you are not allowed to sell these icons and you need to properly attribute Veodesign.

``lib/images/social/square/``
	Square icons for various social sites. This collection comes from the Open Icon Library (homemade) and
	is in the Public Domain. These icons are only 64x64 pixels (all others are 256x256).


How to use these in your templates
----------------------------------

Most of the icons are in 256x256 PNG format.  That is too large for normal usage. Best is to resize the images in your templates using the :ref:`tag-image`.

For example, to display a 64x64 pixel image::

	{% image "lib/images/social/round/email.png" width=64 %}
  
