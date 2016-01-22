.. _cookbook-custom-model:

Create a custom model
=====================

In this chapter we will look at how to implement a model around the
`The Open Movie Database (OMDB) API <http://www.omdbapi.com/>`_.

We will touch useful background information at the same time.

Model modules
^^^^^^^^^^^^^

Models are Erlang modules that are prefixed with ``m_`` and stored in your main module's subdirectory `models`. For example, the model to access Zotonic resources (syntax ``m.rsc.property``) is written in ``models/m_rsc.erl``.

Each model module is required to implement 3 functions (as defined behavior in ``gen_model.erl``):

* m_find_value/3
* m_to_list/2
* m_value/2

m_find_value
^^^^^^^^^^^^

This function fetches a value from a model. Because there are quite some different variation how to use this function, it is good to understand a bit more about the inner workings of data lookup.

Let's start with the parsing of a template expression::

    {{ m.rsc[1].is_cat.person }}

If you have done some work with Zotonic, you will be familiar with this syntax where we can find out if the resource with id ``1`` (the Administrator) is of category Person.

This expression contains 4 parts (separated by dots).

At the very start, the template parser resolves ``m.rsc`` to module ``m_rsc``. The parser then calls ``m_rsc:m_find_value(Key, Source, Context)`` to fetch the value (where Key in our expression has the value of "1").

This is the function specification of ``m_find_value``::

    -spec m_find_value(Key, Source, Context) -> #m{} | undefined | any() when
        Key:: integer() | atom() | string(),
        Source:: #m{},
        Context:: #context{}.

It takes a Key and a data Source - a simple record containing 2 entities, model and value::

    -record(m, {model, value}).

``m_find_value`` often simply returns a value, but it may also return a (modified) data source record for the next call to ``m_find_value``. ``m_rsc.erl`` resolves our expression in 3 different calls to ``m_find_value``, where the data source record ("m" record) is used for pattern matching.

* Step 1: The m record does not contain a value yet. The key (Id) is checked for visibility, and stored in the m record::

    m_find_value(Id, #m{value=undefined} = M, Context) ->
        ...
        M#m{value=RId};

* Step 2: With the m record now containing an Id value, the key ``is_cat`` is found. Again the key is stored in the m record::

    m_find_value(is_cat, #m{value=Id} = M, _Context) when is_integer(Id) ->
        M#m{value={is_cat, Id}};

* Step 3: The next key to parse is ``person``. Since this could have been any category name, a generic ``Key`` variable is used instead of an atom. The result is calculated in a function call, and returned for further manipulation (e.g. filters) or as string to the page::

    m_find_value(Key, #m{value={is_cat, Id}}, Context) ->
        is_cat(Id, Key, Context);


m_to_list
^^^^^^^^^

The second mandatory function transforms a value to a list::

    -spec m_to_list(Source, Context) -> list() when
        Source:: #m{},
        Context:: #context{}.

Not all data models will need to handle lists - in that case the return value is simply the empty list.

Search results are a good example when to apply this function::

    m_to_list(#m{value=#m_search_result{result=undefined}}, _Context) ->
        [];
    m_to_list(#m{value=#m_search_result{result=Result}}, _Context) ->
        Result#search_result.result;
    m_to_list(#m{}, _Context) ->
        [].

Empty models or undefined results return the empty list; valid results are lifted from its record wrapper and returned as a list.

For example, the ``length`` filter makes use of this. It calls ``erlydtl_runtime:to_list`` that calls the model's ``m_to_list``::

    length(Input, Context) ->
        erlang:length(erlydtl_runtime:to_list(Input, Context)).


m_value
^^^^^^^

The final mandatory function specification::

    -spec m_value(Source, Context) -> undefined | any() when
        Source:: #m{},
        Context:: #context{}.

The intended use is to normalize a value to something printable, but you can safely ignore this and return ``undefined``.


Example: Setting up m_omdb
--------------------------

All data calls to the OMDB go through the url ``http://www.omdbapi.com/?``, with query string appended. We can pass the movie id, title, and pass a type (movie/series/episode). OMDB offers more parameters but we don't need them now.


Template interface
^^^^^^^^^^^^^^^^^^

Let's define how will we use the data in templates.

To get all data for a particular ID::

    m.omdb["tt1135300"]

... so that we can get properties like the movie title::

    {{ m.omdb["tt1135300"].title }}

Find an item by title::

    {{ m.omdb["Alien"].year }}

Get all data from a movie::

    {% for k,v in m.omdb.movie["Alien"] %}{{ k }}: {{ v }}{% endfor %}

Get data from a series::

    {{ m.omdb.series[{query title="Dollhouse"}].plot }}

or from an episode::

    {{ m.omdb.episode[{query title="Dollhouse"}].plot }}


Model skeleton
^^^^^^^^^^^^^^

We will write our model in module ``models/m_omdb.erl``. Let's first get the mandatory elements out of the way::

    -module(m_omdb).
    -behaviour(gen_model).

    -export([
        m_find_value/3,
        m_to_list/2,
        m_value/2
    ]).

    -include_lib("zotonic.hrl").

    % ... We will add our m_find_value functions here

    % ... Before ending with the final fallback:
    m_find_value(_, _, _Context) ->
        undefined.

    % This is the default m_to_list if we don't have any list values.
    % We will come back to this in a minute
    m_to_list(_, _Context) ->
        [].

    % The function that we can ignore
    m_value(_, _Context) ->
        undefined.


Querying the API
^^^^^^^^^^^^^^^^

Before diving into the lookup functions, let's see what we want to achieve as result.

1. Using ``m_find_value`` we will generate a list of query parameters, for example ``[{type, "series"}, {title, "Dollhouse"}]``
2. And pass this list to a "fetch data" function
3. That creates a URL from the parameters,
4. loads JSON data from the URL,
5. and transforms the JSON into a property list

The ``fetch_data`` function::

    -spec fetch_data(Query) -> list() when
        Query:: list().
    fetch_data([]) ->
        [{error, "Params missing"}];
    fetch_data(Query) ->
        % Params title or id must be present
        case proplists:is_defined(title, Query) or proplists:is_defined(id, Query) of
            false -> [{error, "Param id or title missing"}];
            true ->
                % Translate query params id, title and type
                % into parameters that OMDB wants
                QueryParts = lists:map(fun(Q) ->
                    make_query_string(Q)
                end, Query),
                Url = ?API_URL ++ string:join(QueryParts, "&"),
                % Load JSON data
                case get_page_body(Url) of
                    {error, Error} ->
                        [{error, Error}];
                    Json ->
                        % Turn JSON into a property list
                        {struct, JsonData} = mochijson2:decode(Json),
                        lists:map(fun(D) ->
                            convert_data_prop(D)
                        end, JsonData)
                end
        end.

It is important to know that we will pass a list, and get a list as result (for other template models this may be different).


Lookup functions
^^^^^^^^^^^^^^^^

To illustrate the simplest ``m_find_value`` function, we add one to get the API url::

    -define(API_URL, "http://www.omdbapi.com/?").

    % Syntax: m.omdb.api_url
    m_find_value(api_url, _, _Context) ->
        ?API_URL;

The functions that will deliver our template interface are a bit more involved. From the template expressions we can discern 2 different patterns:

1. Expressions with 1 part:

  * ``m.omdb["Dollhouse"]``
  * ``m.omdb[{query title="Dollhouse"}]``

2. Expressions with 2 parts:

  * ``m.omdb.series["Dollhouse"]``
  * ``m.omdb.series[{query title="Dollhouse"}]``

When an expression is parsed from left to right, each parsed part needs to be passed on using our m record. For instance with ``m.omdb.series["Dollhouse"]`` we first tranform "series" to ``{type, "series"}``, and then "Dollhouse" to ``{title, "Dollhouse"}``, creating the full query ``[{type, "series"}, {title, "Dollhouse"}]``.

To parse the type, we add these functions to our module::

    % Syntax: m.omdb.movie[QueryString]
    m_find_value(movie, #m{value=undefined} = M, _Context) ->
        M#m{value=[{type, "movie"}]};

    % Syntax: m.omdb.series[QueryString]
    m_find_value(series, #m{value=undefined} = M, _Context) ->
        M#m{value=[{type, "series"}]};

    % Syntax: m.omdb.episode[QueryString]
    m_find_value(episode, #m{value=undefined} = M, _Context) ->
        M#m{value=[{type, "episode"}]};

Notice ``value=undefined`` - this is the case when nothing else has been parsed yet.

The m record now contains a value that will passed to next calls to ``m_find_value``, where we deal with the second part of the expression - let's call that the "query" part.

We can either pass:

1. The movie ID: ``m.omdb["tt1135300"]``
2. The title: ``m.omdb["Alien"]``
3. A search expression: ``m.omdb[{query title="Dollhouse"}]``

Luckily, the movie IDs all start with "tt", so we can use pattern matching to distinguish IDs from titles.

For the ID we recognize 2 situations - with or without a previously found value::

    % Syntax: m.omdb["tt1135300"]
    m_find_value("tt" ++ _Number = Id, #m{value=undefined} = M, _Context) ->
        M#m{value=[{id, Id}]};

    % Syntax: m.omdb.sometype["tt1135300"]
    m_find_value("tt" ++ _Number = Id, #m{value=Query} = M, _Context) when is_list(Query) ->
        M#m{value=[{id, Id}] ++ Query};

In both cases we are passing the modified m record. Because we are retrieving a list, we can leave the processing to ``m_to_list``. For this we need to update our function::

    -spec m_to_list(Source, Context) -> list() when
        Source:: #m{},
        Context:: #context{}.
    m_to_list(#m{value=undefined} = _M, _Context) ->
        [];
    m_to_list(#m{value=Query} = _M, _Context) ->
        fetch_data(Query).

``fetch_data`` will return a property list, so we can write this to get all values::

    {% for k,v in m.omdb["tt1135300"] %}
        {{ k }}: {{ v }}
    {% endfor %}

Handling the title is similar to the ID. Title must be a string, otherwise it would be a property key (atom)::

    % Syntax: m.omdb["some title"]
    m_find_value(Title, #m{value=undefined} = M, _Context) when is_list(Title) ->
        M#m{value=[{title, Title}]};

    % Syntax: m.omdb.sometype["some title"]
    % If no atom is passed it must be a title (string)
    m_find_value(Title, #m{value=Query} = M, _Context) when is_list(Title) ->
        M#m{value=[{title, Title}] ++ Query};


To parse the search expression, we can simply use the readymade property list::

    % Syntax: m.omdb[{query QueryParams}]
    % For m.omdb[{query title="Dollhouse"}], Query is: [{title,"Dollhouse"}]
    m_find_value({query, Query}, #m{value=undefined} = M, _Context) ->
        M#m{value=Query};

    % Syntax: m.omdb.sometype[{query QueryParams}]
    % For m.omdb.series[{query title="Dollhouse"}],
    % Query is: [{title,"Dollhouse"}] and Q is: [{type,"series"}]
    m_find_value({query, Query}, #m{value=Q} = M, _Context) when is_list(Q) ->
        M#m{value=Query ++ Q};


Finally, to handle properties like::

    m.omdb["Alien"].year

... we can no longer pass around the m record; we must resolve it to a value and get the property value::

    % Syntax: m.omdb[QueryString].title or m.omdb.sometype[QueryString].title
    % Key is in this case 'title'
    m_find_value(Key, #m{value=Query} = _M, _Context) when is_atom(Key) ->
        proplists:get_value(Key, fetch_data(Query));

We won't do any validity checking on the parameter here, but for most modules it makes sense to limit the possibilities. See for instance how ``m_search:get_result`` is done.


Full source code
^^^^^^^^^^^^^^^^

The source code of the documentation so far can be found in this gist: `Zotonic template model for the OMDB movie database - source code to accompany the documentation <https://gist.github.com/ArthurClemens/11be71e7fb1b0af31f05>`_.


Possible enhancements
^^^^^^^^^^^^^^^^^^^^^

For a complete model for this API, I would expect:

* Data caching to speed up identical calls
* Support for all API parameters
* Better error handling (the service might be down or return wrong data)

.. seealso::

    * :ref:`models section <guide-models>` in the Developer Guide
    * list of :ref:`all models <models>`.
