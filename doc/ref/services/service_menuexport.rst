
.. include:: meta-menuexport.rst

Given an ``id`` as input argument, returns a JSON export of the
resource's menu structure.

The JSON returned is a nested list of objects which each contain the
pages ``id``, ``url`` and, optionally, any ``children``::
  
  [
    {
        "id": 51253,
        "title": "About",
        "url": "/about"
        "children": [
            {
                "id": 51246,
                "title": "Our team",
                "url": "/about/team"
            },
            {
                "id": 51246,
                "title": "Our work ethics",
                "url": "/about/ethics"
            }
        ]
    },
    {
        "id": 51254,
        "title": "Contact",
        "url": "/contact"
    },
    {
        "id": 50497,
        "title": "Blog",
        "url": "/page/333/blog"
    }
  ]

