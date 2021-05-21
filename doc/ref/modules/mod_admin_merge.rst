
.. include:: meta-mod_admin_merge.rst

Adds functonality to merge two pages together into a single page.

In an interface the *winner* and *loser* can be selected. All connections and properties
from the loser are merged into the winner. Properties of the winner are unchanged.

After merging the loser will be deleted. Visits to the deleted page will be redirected
to the winner page using a *410 Gone*.

On the admin page editor a side panel is added for opening the merge page.
