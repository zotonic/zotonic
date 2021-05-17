
.. include:: meta-sanitize_html.rst

Sanitize a HTML code. Removes elements and attributes that might be dangerous, like ``<script>`` elements.

This filter parses the complete HTML string and ensures that the output is safe and valid HTML.

Do not use (without caching) on busy pages, as the sanitization process is cpu and memory intensive.
