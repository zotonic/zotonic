{# Faceted search - define blocks which will be mapped to facets    #}
{# The facets can be searched using the "facet" query.              #}

{# The block name is used to query the facets, define the type, and #}
{# report the facet back after quering.                             #}
{# A facet called "foo_int" will be called "foo" in searches and    #}
{# reports. Note "is_foo" will still be called "is_foo".            #}

{# The following types are available:                               #}
{# - ..._int          (integer number)                              #}
{# - ..._float        (floating point number)                       #}
{# - ..._date         (datetime in UTC)                             #}
{# - ..._ft           (fulltext search)                             #}
{# - ..._id           (resource id)                                 #}
{# - is_...           (boolean value)                               #}
{# - ..._range_float  (floating point number for minmax reporting)  #}
{# - ..._range_int    (integer number for minmax reporting)         #}
{# - ..._range_date   (date for minmax reporting)                   #}
{# - ..._list         (comma separated list of string values)       #}

{# All others are mapped to text. Texts are always truncated at 100 #}
{# characters for their column index. Full text blocks are using    #}
{# two columns. One for the value, and one for a normalized value.  #}
{# A trigram index is added on the normalized column.               #}

{# Date(time)s are stored as-is, no additional timezone conversion  #}
{# is done, so be careful with the timezone used to fill the block. #}

{% block category_id %}{{ id.category_id }}{% endblock %}

{% block foo_ft %}{% endblock %}

{% block bar_list %}{% endblock %}
