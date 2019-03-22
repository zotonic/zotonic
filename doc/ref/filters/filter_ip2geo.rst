
.. include:: meta-ip2geo.rst

Maps an IP address to information about that country.

The module ``mod_geoip`` must be enabled to use this filter.

Example, print the information for the current visitor::

  {% print m.req.peer|ip2geo %}

Might print (depending on the visitor’s IP address)::

    #{city =>
          {trans,[{zh,<<230,160,188,231,189,151,229,174,129,230,
                        160,185>>},
                  {ru,<<208,147,209,128,208,190,208,189,208,184,208,189,
                        208,179,208,181,208,189>>},
                  {pt,<<"Groningen">>},
                  {ja,<<227,131,149,227,131,173,227,131,188,227,131,139,
                        227,131,179,227,130,178,...>>},
                  {fr,<<"Groningue">>},
                  {es,<<"Groninga">>},
                  {en,<<"Groningen">>},
                  {de,<<"Groningen">>}]},
      continent =>
          #{code => <<"EU">>,
            name =>
                {trans,[{zh,<<230,172,167,230,180,178>>},
                        {ru,<<208,149,208,178,209,128,208,190,208,191,208,176>>},
                        {pt,<<"Europa">>},
                        {ja,<<227,131,168,227,131,188,227,131,173,227,131,131,
                              227,131,145>>},
                        {fr,<<"Europe">>},
                        {es,<<"Europa">>},
                        {en,<<"Europe">>},
                        {de,<<"Europa">>}]}},
      country =>
          #{is_eu => true,iso => <<"nl">>,
            name =>
                {trans,[{zh,<<232,141,183,229,133,176>>},
                        {ru,<<208,157,208,184,208,180,208,181,209,128,208,187,
                              208,176,208,...>>},
                        {pt,<<"Holanda">>},
                        {ja,<<227,130,170,227,131,169,227,131,179,227,131,128,
                              231,...>>},
                        {fr,<<"Pays-Bas">>},
                        {es,<<"Holanda">>},
                        {en,<<"Netherlands">>},
                        {de,<<"Niederlande">>}]}},
      location =>
          #{accuracy_radius => 100,latitude => 53.2124,
            longitude => 6.5538,postcode => <<"9726">>,
            timezone => <<"Europe/Amsterdam">>},
      subdivisions =>
          [#{code => <<"GR">>,
             name =>
                 {trans,[{ru,<<208,147,209,128,208,190,208,189,208,184,
                               208,189,208,179,...>>},
                         {pt,<<"Groninga">>},
                         {fr,<<"Groningue">>},
                         {es,<<"Groninga">>},
                         {en,<<"Groningen">>},
                         {de,<<"Groningen">>}]}}]}}

The ``trans`` records can be shown directly, Zotonic will select the correct language::

    {% with m.req.peer|ip2geo as info %}
        {{ info.country.name }} {% if info.city %} - {{ info.city }}{% endif %}
    {% endwith %}

This might print (depening on the peer)::

    Netherlands - Groningen

If the IP address could not be mapped then ``undefined`` is returned.
