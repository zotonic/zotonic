
.. include:: meta-mod_search.rst

`mod_search` implements various ways of searching through the main resource table.
The following searches are implemented in `mod_search`:

Name	Description	Required arguments
featured	List of pages, featured ones first.	
featured	List of pages in a category, featured ones first.	cat
featured	List of pages in a category having a certain object, featured pages first.	cat, object, predicate
latest	The newest pages.	
latest	The newest pages within in a category.	cat
upcoming	Pages with future date_end, sorted on ascending date_start.	cat
autocomplete	Full text search where the last word gets a wildcard.	text
autocomplete	Full text search where the last word gets a wildcard, filtered by category.	cat, text
fulltext	Full text search. Returns {id,score} tuples.	text
fulltext	Full text search, filtered by category. Returns {id,score} tuples.	cat, text
referrers	All subjects of a page.	id
media_category_image	All pages with a medium and within a certain category. Used to find category images.	cat
media_category_depiction	All pages with a depiction edge to an image. Used to find category images.	cat
media	All pages with a medium, ordered by descending creation date.	
all_bytitle	Return all {id,title} pairs for a category, sorted on title.	cat
all_bytitle_featured	Return all {id,title} pairs for a category, sorted on title, featured pages first	cat
all_bytitle	Return all {id,title} pairs for a category without subcategories, sorted on title.	cat_is
all_bytitle_featured	Return all {id,title} pairs for a category without subcategories, sorted on title, featured pages first.	cat_is
match_objects	Returns a list of pages with similar object ids to the objects of the given rsc with id id.  Returns {id, rank}. Accepts optional cat parameters for filtering on category.	id
match_objects_cats	Returns a list of pages with similar object ids or categories.  Returns {id, rank}. Accepts optional cat parameters for filtering on category.	

id
query	

Very powerful search with which you can implement almost all of the other search functionality.

The documentation page of m.search[{query}]
	
archive_year
	Returns an overview on publication year basis, for a specified category. Every row returned has parts: "as_date", "year" and "count". The order is descending, newest year first. (since 0.7)
	cat
archive_year_month	Return a grouped "archive" overview of resources within a category. The result is a double list, consisting of [ {year, [ months ] }]. The result is grouped on publication year and month, and includes counts. The order is descending, newest year first.	cat
keyword_cloud	Return a list of (keyword_id, count) for all resources within a given category. The list is ordered on keyword title.	cat
previous	Given an id, return a list of "previous" ids in the given category. This list is ordered by publication date, latest first.	id, cat
next	Given an id, return a list of "next" ids in the given category. This list is ordred by publication date, oldest first.	id, cat


.. seealso:: :ref:`manual-datamodel-query-model`


