{#
    This file is for checking the syntax highlighting of zotonic templates.

    Any and all constructs that ought to be highlighted in a certain way
    should be in this file, although the verification is thus far only
    manual using your very own set of eyes.

#}

1. Highlight tags with // in a attribute value

<script src="http://example.com/dummy.js"></script>

Check: that both the opening and closing script tags above are
highlighted the same.

2. Highlight tag in spite of dot in tag contents

{% extends "foo.tpl" %}
