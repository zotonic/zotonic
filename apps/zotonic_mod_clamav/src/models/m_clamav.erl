%% @doc ClamAV availability for the admin dashboard.
-module(m_clamav).
-moduledoc("
The `m.clamav.is_available` model path checks whether ClamAV responds to a ping,
using the configured Unix socket with TCP fallback. Requires permission to use
`mod_admin`. The dashboard loads this check asynchronously on each page load.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/is_available/...` | Return whether the virus scanner responds. |

").

-behaviour(zotonic_model).

-export([m_get/3]).

-spec m_get(Path, Msg, Context) -> Result
    when
        Path :: list(),
        Msg :: zotonic_model:opt_msg(),
        Context :: z:context(),
        Result :: zotonic_model:return().
m_get([ <<"is_available">> | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_admin, Context) of
        true -> {ok, {z_clamav:ping() =:= pong, Rest}};
        false -> {error, eacces}
    end;
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.
