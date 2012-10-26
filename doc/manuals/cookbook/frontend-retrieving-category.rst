Retrieving the category of a page
=================================

Getting the category from a URL is somewhat involved, but not
impossible. This is an example of what you can do with filters.

Why
---

It is often useful to use a page's category to present it on the page
itself or to look up related content.  This guide provides
step-by-step instructions for getting a page's category in a template.

I have a page with url of the form ``/my_category``,
``/my_category/:id``, or ``/my_category/:id/:slug``. How can I
retrieve the category from the url?

Assumptions
-----------

Readers are assumed to be comfortable with template development.

How
---

Since category is a direct property of m_rsc we can directly access it::

  {% with id.category as my_cat %} 
    ...
  {% endwith %}
