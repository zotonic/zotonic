OAuth Authentication and API Access Control
===========================================

This module provides two different kinds of OAuth services:

 1. Access control for APIs using OAuth tokens
 2. Authentication with external identity providers (LinkedIn, Twitter, Facebook, etc.)


## API access control

This is handled with a _Bearer_ token in the _Authorization_ HTTP header.

There is not yet an admin interface to make generate the tokens.

However, there is an Erlang API:

```erlang
SudoContext = z_acl:sudo(Context),

% Make a new OAuth2 token with full access to the admin (user 1) account
TokenProps = #{
    <<"is_read_only">> => false,
    <<"is_full_access">> => true
},

% Insert the token rights into the database, couple it to user 1.
% The returned id is a simple serial number.
{ok, TokenId} = m_oauth2:insert(1, TokenProps, SudoContext),

% Generate a token, it is valid for 3600 seconds (pass 'undefined' if no expiration).
% This token is a signed and encrypted binary that can be used in the
% Authorization header.
{ok, Token} = m_oauth2:encode_bearer_token(TokenId, 3600, SudoContext),
```

Use the token to make an API request:

```erlang
Url = z_context:abs_url( z_dispatcher:url_for(api, [ {star, <<"model/acl/get/user">> } ], Context), Context),

{ok, {_, _, _, Body}} = z_url_fetch:fetch(Url, [ {authorization, <<"Bearer ", Token/binary>>} ]),

% The user-id associated should be '1', as was defined when inserting the token.
#{ <<"result">> := 1, <<"status">> := <<"ok">> } = jsxrecord:decode(Body),
```


## Authentication with identity providers

Uses OAuth2 to:

 * Sign up users using an external (trusted) identity service
 * Log in using the external service
 * Associate an external service user with an existing user


### Steps for the OAuth redirections to the external service

 1. Click on button, this opens a new window
 2. In the window load a page, load a page that will redirect to the correct service:

    `{% url logon_service service='twitter' is_connect=is_connect %}`

    This URL and template are proviced by `mod_authentication`.
    The page then includes a template that will perform the correct redirect, the
    name of the page is `"_logon_service."++q.service++".tpl"`

    This service template should be provided by the module implementing the given service.
 3. The service template is loaded lazy, so that the correct `z.auth` cookie can be set
    for this page. This is needed as the server will store information in the server
    side session storage.
 4. Authentication at the external service
 5. Redirect back to our server, using the url `oauth-service/redirect`
    This displays a new page, which (after reconnecting with the server) will send a message
    to the server. This message includes all _query_ arguments passed to the URL.
 6. The server checks the passed query arguments and then:

    a. If log in and all ok: server posts to auth-client-model to re-authenticate
       using a special one-time token. This sets a new z.auth cookie and forces all tabs
       to re-authenticate using the new user id. After the identity is changed the popup
       window is closed (using a data attribute on the body tag, handled by the ui model).

    b. If connect and all ok: the identity is added to the current user-id, popup-window is closed.

    c. If log in but needs confirmation: render a confirm template. The confirm button will
       send a message to the server to create the new user and then the server proceeds to
       step (a).

    d. On an error: display an error message.

 7. Any browser tab that was open will get a message via the ServiceWorker that the authentication
    has been changed. This fill will force a re-sync using the new z.auth cookie.

Problems:

 * Race-condition setting the new z.auth cookie and refresh by other tabs of the same cookie.
   This is a generic problem, that should be solved in the auth client model.

