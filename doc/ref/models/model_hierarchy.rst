
.. include:: meta-hierarchy.rst

The category hierarchy tables have been replaced by *m_hierarchy*.
This model defines named hierarchies of resources (pages).

If the categories are changed then the system needs to update the
*pivot_category_nr* field of all resources. With the introduction 
of *m_hierarchy* this renumbering is much more efficient and will 
only affect a minimal number of resources.

The *m_hierarchy* module is also used for the content- and user group
hierarchies, as used by the new *mod_acl_user_groups* module.
