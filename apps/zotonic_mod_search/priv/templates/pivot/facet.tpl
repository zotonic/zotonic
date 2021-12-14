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

{# All others are mapped to text. Texts are always truncated at 80  #}
{# characters for their column index. Full text blocks are using    #}
{# two columns. One for the value, and one for a normalized value.  #}
{# A trigram index is added on the normalized column.               #}

{# Date(time)s are stored as-is, no additional timezone conversion  #}
{# is done, so be careful with the timezone used to fill the block. #}

{# If you need to return a label different than the value, then a   #}
{# block with prefix "label_" can be added. This block is rendered  #}
{# with the id of a resource that generated the given value.        #}
{# For blocks with postfix "_id" the default label is the resource  #}
{# title, or for persons the rendering of "_name.tpl"               #}

{% block category_id %}{{ id.category_id }}{% endblock %}

{% block org_pubdate_range_date %}{{ id.org_pubdate|default:id.publication_start }}{% endblock %}

{% block org_pubyear_int %}{{ id.org_pubdate|default:id.publication_start|date:"Y" }}{% endblock %}

{% block created_year_int %}{{ id.created|date:"Y" }}{% endblock %}