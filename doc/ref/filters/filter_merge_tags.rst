.. highlight:: django
.. include:: meta-merge_tags.rst

A mail-merge like filter where tag-expressions in a text are replaced with the
value of their evaluation.

The tags in the text are surrounded by ``{{ ... }}`` markers. After the tag
is evaluated the resulting text is escaped and replaces the tag.

Example::

    {{ "Hello {{ name }}"|merge_tags:%{ name: "World" } }}

This is most useful to replace markers in texts like emails, where the default
text is saved into a body or summary of a page.

For example, a page’s body could be like::

    <p>Hello {{ name_first }},</p>

And the template could be like::

    {{ message_id.body|merge_tags:%{ id: recipient_id } }}

Then the tag in the saved body will be replaced with the first name of the recipient.

It is also possible to use some simple filters::

    <p>Hello {{ name_first|default:title }},</p>

Or simple expressions::

    <p>The sum is {{ 100 + 200 }}</p>

Or a date filter::

    <p>This article was created on {{ created|date:"Y-m-d" }}</p>

