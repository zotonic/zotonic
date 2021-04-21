
.. include:: meta-toc.rst

Filter to derive a Table Of Contents from a HTML body.

This filter extracts a nested table of contents from the h2..h6 elements in a HTML text.

The headers may not have any attributes (ie. only ``<h2>``).

All sections are wrapped in ``<div>`` elements, this to make it possible to make the
headers sticky without having them overlap.

.. highlight:: django

Example usage::

    {% with id.body|toc as toc, body %}
        {% include "page-parts/_toc.tpl" toc=toc %}
        {{ body|show_media }}
    {% endwith %}

.. highlight:: erlang

The returned ToC tree is a nested tree of 3-tuples::

    [ {<<"toc1">>, <<"Text of header">>, [ ... ]}, ... ]

The third element is a list of sub-headers.

.. highlight:: django

To generate a nested table of contents from the above structure::

    {% include "page-parts/_toc.tpl" toc=toc %}

And then the ``page-parts/_toc.tpl`` as::

    {% with level|default:1 as level %}
    {% if toc %}
        <ol class="toc-level-{{ level }}">
        {% for p, text, sub in toc %}
            <li>
                <a href="#{{ p }}">{{ text }}</a>
                {% include "page-parts/_toc.tpl" toc=sub level=level+1 %}
            </li>
        {% endfor %}
        </ol>
    {% endif %}
    {% endwith %}
