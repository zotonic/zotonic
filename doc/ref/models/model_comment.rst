
.. include:: meta-comment.rst

Accesses comments on a page.

Comments are stored in the ``comment`` table. Comments are no separate
rsc records because that will add many extra records and also because
of access control restrictions.

When a page is not visible to a certain user then its comments
shouldn't be visible as well. To simplify this check the comments are
placed separate and made part of the rsc record.

This separate comment table also helps with cleaning up comments when
the rsc record is deleted.

.. todo:: Finish m_comment
