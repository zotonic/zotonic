
.. include:: meta-countries.rst

Returns an array of countries as installed by :ref:`mod_geomap`, with
geographical coordinates and outline area.

Every country has the following JSON object returned::
  
  {'geometry': {'coordinates': [[[[35.26, 31.79],
                                                 [35.25, 31.79],
                                                 [35.25, 31.81],
                                                 [35.26, 31.79]]],
                                               [[[35.62, 33.25],
                                                 [35.65, 32.69],
                                                 [35.55, 32.39],
                                                 [35.28, 32.52],
                                                 [34.88, 31.39],
                                                 [35.48, 31.5],
                                                 [34.98, 29.55],
                                                 [34.9, 29.49],
                                                 [34.27, 31.22],
                                                 [34.33, 31.26],
                                                 [34.49, 31.6],
                                                 [35.1, 33.09],
                                                 [35.62, 33.25]]]],
                              'type': 'MultiPolygon'},
                'id': 376,
                'properties': {'colour': '#ccc',
                                'name': 'Israel',
                                'value': ''},
                'type': 'Feature'},
