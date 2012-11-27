
.. include:: meta-export.rst

Exports a "dump" of the given :term:`resource`. A dump is a JSON
object with `rsc`, `category`, `media`, `edges` sub-objects. When
requesting the service::

  http://localhost:8000/api/base/export?id=325

The following might be returned::

  {
    "id": 325,
    "uri": "http://localhost:8000/en/id/325",
    "rsc": {
        "category_id": 106,
        "created": "2012-10-25 16:18:08",
        "creator_id": 1,
        "id": 325,
        "is_authoritative": true,
        "is_featured": false,
        "is_protected": true,
        "is_published": true,
        "modified": "2012-10-25 16:18:08",
        "modifier_id": 1,
        "name": "wordpress_1",
        "page_path": null,
        "pivot_geocode": null,
        "publication_end": "undefined",
        "publication_start": "2008-04-15 09:28:00",
        "slug": "a-somewhat-brief-history",
        "uri": null,
        "version": 6,
        "visible_for": 0,
        "date_end": null,
        "date_start": null,
        "title": "A (somewhat) brief history",
        "body": "<p>...</p>",
        "summary": "",
        "custom_slug": true,
        "installed_by": "mod_import_wordpress"
    },
    "edges": [
        {
            "predicate_id": 301,
            "predicate_name": "author",
            "object_id": 321,
            "seq": 1000000,
            "predicate_uri": "http://localhost:8000/en/id/301",
            "predicate_title": {
            "trans": {
                "en": "Author",
                "nl": "Auteur"
             }
            },
            "object_uri": "http://localhost:8000/en/id/321",
            "object_title": "jlueck"
        },
        ...
     ]
  }

