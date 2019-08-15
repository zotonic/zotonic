
This module contains fonts, icons and other _art work_ that can be used
in sites and modules.

Icon fonts included:

 * Font Awesome 4 (build from sources)
 * Font Awesome 3 (no sources)
 * Material Design (build from sources)

Images included:

 * Emotes: Tango Icon Library
 * Flags: Open Icon Library
 * Mime Icons: Tango Icon Library
 * Noun: The Noun Project
 * Social icons round: Veo Design
 * Social icons square: Open Icon Library
 * Zotonic logos

## Licenses

All files in this module have their own license.

Most are either licensed under Public Domain or with a creative commons license.
If you want to sell this collection, then you have to delete some.

All licenses allow copying, non-commercial and commercial use.

Check the individual licenses for details.

The Zotonic logos are licensed under the APLv2.


## Updating Font Awesome 4

If you update font-awesome-4 then in:

    prib/lib-src/font-awesome-4/less/variables.less

Set the following variable:

    @fa-font-path:        "/lib/font-awesome-4/fonts";

Also keep the Makefile in place.

## Updating Material Design

If you update Material Design then in:

    prib/lib-src/material-design/less/variables.less

Set the following variable:

    @md-font-path:        "/lib/material-design/fonts";

Also keep the Makefile in place.

