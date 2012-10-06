.. include:: meta-script.rst

script
======

Placeholder for generated Javascript.

Zotonic generates Javascript for the actions and other template logic. This script needs to be added to the page.  The ``{% script %}`` scomp designates the place where the ``<script>`` element with all the generated javascript can be placed.

Normally the ``{% script %}`` scomp is placed at the end of the page, just above the ``</body>``.

Note that all Javascripts generated after the ``{% script %}`` scomp will not be included in the generated page. Only a single ``{% script %}`` scomp is allowed on any page.

Example::

      {# at the bottom of the template ... #}
      {% script %}
    </body>
  </html>

This will generate something similar to::

  <script type='text/javascript'>
  $(function() {
  z_pageid="MouKz4PgrcROM5efU8iL";
  z_postback_loop();

  $('#vtretq').bind('click', function(event) { window.location = "/admin/edit/647"; return z_opt_cancel(this); } );
  z_init_postback_forms();
  z_default_form_postback = "bVcYISt9JOG/AQZkP9SOZmc//GqDaAVrAAZzdWJtaXRkAAl1bmRlZmluZWRkAAl1bmRlZmluZWRqZAANcmVzb3VyY2VfcGFnZQ==";
  });
  </script>
    </body>
  </html>

Note that the contents of this block will be completely different per page.

The script scomp can have the following arguments:

+-----------+------------------------------------------------------------+---------------+
| Argument  | Description                                                |	Example      |
+-----------+------------------------------------------------------------+---------------+
| nostartup	| Exclude the page initialization code from the script, only | nostartup     |
|           | includes the scripts from actions etc.  Default is to      |               |
|           | include the page initialization code.	                     |               |
+-----------+------------------------------------------------------------+---------------+
| format    | Select a different format than the ``<script/>`` tag.  For | format="html" |
|           | now this only accepts ``"html"`` (for the ``<script/>``    |               | 
|           | tag) and ``"escapejs"`` for an escaped javascript string.  |               |
|           | Default is ``"html"``.	                                   |               |
+-----------+------------------------------------------------------------+---------------+


