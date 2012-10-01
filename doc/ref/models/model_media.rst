
.. include:: meta-media.rst

Access to data about uploaded files and other media.

The ``medium`` (singular form of media) table stores all information
of uploaded files or other media. Every resource can contain a single
medium. A resource with a medium is most often of the category image, audio,
video or document.

In template the ``m_media`` model is used to fetch the medium record
by the resource id: ``m.media[id]``. This is the same function as with
``m.rsc[id].medium`` except, that the m_rsc model does access control
checks and the m_media does not.

The ``m_media`` model implements all functions to handle media files
and is used by other Erlang modules.


Properties of a medium record
-----------------------------

A medium record has minimally the following properties, other
properties can be added by modules.

+--------------------+-------------------------------+--------------------------------------+
|Property            |Description                    |Example value                         |
+====================+===============================+======================================+
|id                  |Id of the medium record, equal |512                                   |
|                    |to the page id.                |                                      |
+--------------------+-------------------------------+--------------------------------------+
|filename            |Filename and path of the       |<<"2009/10/20/zotonic-datamodel.jpg">>|
|                    |uploaded file, relative to the |                                      |
|                    |archive directory.             |                                      |
+--------------------+-------------------------------+--------------------------------------+
|rootname            |Root name of the filename.     |<<"zotonic-datamodel">>               |
+--------------------+-------------------------------+--------------------------------------+
|original_filename   |Filename as suggested by the   |<<"Zotonic-datamodel.jpg">>           |
|                    |user agent when uploading the  |                                      |
|                    |file. Can contain illegal      |                                      |
|                    |characters.                    |                                      |
+--------------------+-------------------------------+--------------------------------------+
|mime                |Mime type of the medium.       |<<"image/jpeg">>                      |
+--------------------+-------------------------------+--------------------------------------+
|width               |Width in pixels.               |536                                   |
+--------------------+-------------------------------+--------------------------------------+
|height              |Height in pixels.              |737                                   |
+--------------------+-------------------------------+--------------------------------------+
|orientation         |Exif oritentation of the image.|1                                     |
+--------------------+-------------------------------+--------------------------------------+
|sha1                |Optional sha1 checksum of      |                                      |
|                    |uploaded file. Undefined when  |                                      |
|                    |not present.                   |                                      |
+--------------------+-------------------------------+--------------------------------------+
|size                |Size in bytes of the uploaded  |71585                                 |
|                    |file.                          |                                      |
+--------------------+-------------------------------+--------------------------------------+
|preview_filename    |Optional filename for a        |                                      |
|                    |generated file preview.        |                                      |
+--------------------+-------------------------------+--------------------------------------+
|preview_width       |Optional. Width of the         |                                      |
|                    |generated preview.             |                                      |
+--------------------+-------------------------------+--------------------------------------+
|preview_height      |Optional. Height of the        |                                      |
|                    |generated preview.             |                                      |
+--------------------+-------------------------------+--------------------------------------+
|is_deletable_file   |If the file should be deleted  |true                                  |
|                    |when the medium record is      |                                      |
|                    |deleted. A boolean.            |                                      |
+--------------------+-------------------------------+--------------------------------------+
|is_deletable_preview|If the optionally generated    |false                                 |
|                    |preview file should be deleted |                                      |
|                    |when the medium record is      |                                      |
|                    |deleted. A boolean.            |                                      |
+--------------------+-------------------------------+--------------------------------------+
|created             |Timestamp when the medium      |{{2009,10,20},{13,47,27}}             |
|                    |record is created.             |                                      |
+--------------------+-------------------------------+--------------------------------------+
