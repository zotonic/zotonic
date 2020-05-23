.. include:: meta-sanitize_embed_url.rst

This notification is used to sanitize and whitelist *embed urls* passed with the
media import routines.

.. highlight:: erlang

Example usage in a module, where URLs from some public broadcasters are whitelisted::

    -export([
        observe_sanitize_embed_url/2
    ]).

    observe_sanitize_embed_url(#sanitize_embed_url{hostpath= <<"media.vara.nl/", _/binary>> = Url}, _Context) ->
        Url;
    observe_sanitize_embed_url(#sanitize_embed_url{hostpath= <<"biografie.vara.nl/", _/binary>> = Url}, _Context) ->
        Url;
    observe_sanitize_embed_url(#sanitize_embed_url{hostpath= <<"js.vpro.nl/", _/binary>> = Url}, _Context) ->
        Url;
    observe_sanitize_embed_url(#sanitize_embed_url{hostpath= <<"embed.vpro.nl/", _/binary>> = Url}, _Context) ->
        Url;
    observe_sanitize_embed_url(_, _Context) ->
        undefined.


Note the ``undefined`` returned if no other patterns match. This allows other modules to
check for different patterns.

