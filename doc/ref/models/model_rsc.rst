
.. include:: meta-rsc.rst

The main resource model. This model provides an interface to all
resource ("page") information. It also provides an easy way to fetch
edges from pages without needing to use the :ref:`model-edge` model.  

Properties
----------

All stored texts are `HTML escaped`, except for the body text which is
stored as-is and assumed to be valid XHTML. Retrieved texts are always
binaries. Dates are stored as a standard Erlang date time tuple, for
example {{2008,12,10},{15,30,00}}. Retrieved dates are in the local
time of the server.


A resource has the following properties accessible from the templates:

+-------------------+-----------------------------------------------------+--------------------------------+
|Property           |Description                                          |Example value                   |
+===================+=====================================================+================================+
|id                 |Id of the page, an integer.                          |42                              |
+-------------------+-----------------------------------------------------+--------------------------------+
|title              |Title of the page. Returns a binary.                 |<<"Breaking News">>             |
|                   |                                                     |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|short_title        |Short title of the page. Used in menus. Returns a    |<<"News!">>                     |
|                   |binary.                                              |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|summary            |Summary of the page. Returns a binary or undefined.  |<<"Page summary.">>             |
+-------------------+-----------------------------------------------------+--------------------------------+
|body               |The HTML body of a page. Returns a binary or         |<<"<p>Hello</p>">>              |
|                   |undefined.                                           |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|date_start         |Start date when the page has a period. Examples are  |{{2008,12,10},{15,30,00}}       |
|                   |events or the birth date of a person. Returns a      |                                |
|                   |datetime tuple or undefined.                         |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|date_end           |End date when the page has a period. Returns a       |{{2009,12,5},{23,59,59}}        |
|                   |datetime tuple or undefined. When there is a start   |                                |
|                   |date then there is also an end date.                 |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|name               |Unique name of the page. Returns a binary of         |<<"page_home">>                 |
|                   |undefined. Valid characters are a-z, 0-9 and _       |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|page_path          |Unique path of the page, used for url                |<<"/">>                         |
|                   |generation. Returns a binary or undefined. Valid     |                                |
|                   |characters are a-z, 0-9, / and -                     |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|page_url           |The url of the page. Derived using the page’s        |[$/, "blog", [$/, "42"]]        |
|                   |category, the page id and its slug. Returns a non    |                                |
|                   |flattened list. Returns the binary page_path when it |                                |
|                   |is set.                                              |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|default_page_url   |The page without considering its page_path setting.  |[$/, "page", [$/, "42", [$/,    |
|                   |                                                     |"my-slug"]]                     |
+-------------------+-----------------------------------------------------+--------------------------------+
|is_authoritative   |Whether this page originated on this site or is      |true                            |
|                   |imported and maintained on another site. Return a    |                                |
|                   |boolean.                                             |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|uri                |The absolute unique uri of this resource. Refers to  |<<"http://example.com/id/42">>  |
|                   |the “id” dispatch rule for authoritative (local)     |                                |
|                   |resources. Returns a binary.                         |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|category_id        |Id of the category the page belongs to. Returns an   |102                             |
|                   |integer.                                             |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|category           |Category record the page belongs to. Returns a       |[{id,102},{parent_id,undefined],|
|                   |property list.                                       |... ,{name, <<"person">>}]      |
+-------------------+-----------------------------------------------------+--------------------------------+
|seo_noindex        |Whether to let search engines index this             |false                           |
|                   |page. Returns a boolean or undefined.                |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|seo_title          |Title for on top of the browser window. Returns a    |<<"Welcome Title">>             |
|                   |binary or undefined.                                 |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|slug               |Slug used for url generation, appended to page       |<<"the-world-is-flat">>         |
|                   |urls. Binary or undefined. Valid characters are a-z, |                                |
|                   |0-9 and -                                            |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|seo_keywords       |Keywords for search engine optimization. List of     |<<"world, model, flat">>        |
|                   |keywords separated with commas. Returns a binary or  |                                |
|                   |undefined.                                           |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|seo_desc           |Page description for search engines. Returns a binary|<<"The truth about the world's  |
|                   |or undefined.                                        |shape">>                        |
+-------------------+-----------------------------------------------------+--------------------------------+
|is_me              |Check if this page is the current user’s person      |false                           |
|                   |page. Returns a boolean.                             |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|is_visible         |Check if this page is visible for the current        |true                            |
|                   |user. Returns a boolean.                             |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|is_editable        |Check if this page is editable by the current        |false                           |
|                   |user. Returns a boolean.                             |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|is_ingroup         |Check if the current user is a member of the group   |true                            |
|                   |the page belongs to. Returns a boolean.              |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|exists             |Check if the page exists. Useful when checking if a  |true                            |
|                   |named page is present or not. Returns a boolean.     |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|is_a               |Returns a list of the category hierarchy the page    |[{text,true}, {article,true}]   |
|                   |belongs to. The list is suitable for indexing with   |                                |
|                   |category atoms.                                      |                                |
|                   |                                                     |                                |
|                   |Example usage: {{ m.rsc[id].is_a.article }}          |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|is_cat             |Direct check if a page is a certain category. More   |true                            |
|                   |efficient then is_a.                                 |                                |
|                   |                                                     |                                |
|                   |Example usage: {{ m.rsc[id].is_cat.person }}         |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|is_featured        |If featured checked or not. Returns a boolean        |false                           |
+-------------------+-----------------------------------------------------+--------------------------------+
|is_protected       |If this page is protected from deletion. Returns a   |false                           |
|                   |boolean                                              |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|is_published       |If this page has been published. Returns a boolean   |true                            |
+-------------------+-----------------------------------------------------+--------------------------------+
|publication_start  |Start date of the publication period. Returns a      |{{2009,12,24},{9,0,0}}          |
|                   |datetime tuple.                                      |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|publication_end    |End date of the publication period. Returns a        |{{9999,8,17},{12,0,0}}          |
|                   |datetime tuple.                                      |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|visible_for        |Visibility level. Returns an integer. 0 = world      |0                               |
|                   |visible, 1 = only for logged on users, 2 = only for  |                                |
|                   |group members, 3 = only for current user.            |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|o                  |Used to access the objects of page: the pages this   |fun(Predicate,Context)          |
|                   |page refers to. Returns a function which should be   |                                |
|                   |indexed with the edge’s predicate name (atom). When  |                                |
|                   |indexed the function will return a list of integers. |                                |
|                   |                                                     |                                |
|                   |``Example usage: {{ m.rsc[id].o.author[1].title }}`` |                                |
|                   |                                                     |                                |
|                   |This returns the first author that is linked from    |                                |
|                   |this page.                                           |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|s                  |Access the subjects of a page: the pages that are    |fun(Predicate,Context)          |
|                   |referring to this page. Returns a function which     |                                |
|                   |should be indexed with the edge’s predicate name     |                                |
|                   |(atom). When indexed the function will return a list |                                |
|                   |of integers.                                         |                                |
|                   |                                                     |                                |
|                   |Example usage: {{ m.rsc[id].s.author[1].title }}     |                                |
|                   |                                                     |                                |
|                   |This returns the first article that links to me with |                                |
|                   |a author connection.                                 |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|op                 |Returns a list of all predicates on edges from this  |[about, related]                |
|                   |page. The predicates are atoms.                      |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|sp                 |Returns a list of all predicates on edges to this    |[author]                        |
|                   |page. The predicates are atoms.                      |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|predicates_edit    |Returns a list of all allowed predicates from this   |[308,300,304,303,302,300]       |
|                   |page. Used for editing the page. Returns a list of   |                                |
|                   |predicate ids (in contrast with the atoms of op and  |                                |
|                   |sp).                                                 |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|media              |Return a list of all media ids connected to the      |[842,3078]                      |
|                   |page. The media are connected with the predicate     |                                |
|                   |“depiction”.                                         |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|medium             |Return a property list describing the file or medium |[ {id,512}, {filename,          |
|                   |attached to the page. A medium record is present for |<<"2009/1…">>, … ]              |
|                   |pages that are an image, video etc. Returns undefined|                                |
|                   |when there is no medium defined. See the model       |                                |
|                   |m_media for more information.                        |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|depiction          |Return the medium record that can be used for the    |[ {id,512}, {filename,          |
|                   |image of a page. Either returns a medium page        |<<"2009/1…">>, … ]              |
|                   |attached to the page or the medium record of the page|                                |
|                   |itself. When no medium is found then undefined is    |                                |
|                   |returned.                                            |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|email              |E-mail address. Returns a binary or undefined.       |<<"me@example.com">>            |
+-------------------+-----------------------------------------------------+--------------------------------+
|website            |URL of a website. Returns a binary or undefined.     |<<"http://zotonic.com">>        |
+-------------------+-----------------------------------------------------+--------------------------------+
|phone              |Phone number. Returns a binary or undefined.         |<<"+31201234567">>              |
+-------------------+-----------------------------------------------------+--------------------------------+
|phone_alt          |Alternative phone number. Returns a binary or        |undefined                       |
|                   |undefined.                                           |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|phone_emergency    |Phone number to call in emergencies.                 |<<"112">>                       |
+-------------------+-----------------------------------------------------+--------------------------------+
|address_street_1   |Address line 1. Returns a binary or undefined.       |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|address_street_2   |Address line 2. Returns a binary or undefined.       |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|address_city       |City part of address. Returns a binary or undefined. |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|address_postcode   |Postcode part of address. Returns a binary or        |                                |
|                   |undefined.                                           |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|address_state      |State part of address. Returns a binary or undefined.|                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|address_country    |Country part of address. Returns a binary or         |                                |
|                   |undefined.                                           |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|mail_street_1      |Mailing address line 1. Returns a binary or          |                                |
|                   |undefined.                                           |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|mail_street_2      |Mailing address line 2. Returns a binary or          |                                |
|                   |undefined.                                           |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|mail_city          |City part of mailing address. Returns a binary or    |                                |
|                   |undefined.                                           |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|mail_postcode      |Postcode part of mailing address. Returns a binary or|                                |
|                   |undefined.                                           |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|mail_state         |State part of mailing address. Returns a binary or   |                                |
|                   |undefined.                                           |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|mail_country       |Country part of mailing address. Returns a binary or |                                |
|                   |undefined.                                           |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|name_first         |First name of person. Returns a binary or undefined. |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|name_middle        |Middle name of person. Returns a binary of undefined.|                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|name_surname_prefix|Prefix for the surname of a person. Returns a binary |<<"van der"">, <<"von">>        |
|                   |or undefined.                                        |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|name_surname       |Surname or family name of person. Returns a binary or|                                |
|                   |undefined.                                           |                                |
+-------------------+-----------------------------------------------------+--------------------------------+

.. seealso:: :ref:`model-edge`, :ref:`model-media`
