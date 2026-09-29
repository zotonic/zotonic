%% @doc ClamAV availability for the admin dashboard.
-module(m_clamav).
-moduledoc(#{
    zotonic_keywords => ["reference", "backend_developer", "model", "malware_scanning", "monitor", "security"]
}).
-moduledoc("
Reports whether the ClamAV daemon (`clamd`) is available. The
`m.clamav.is_available` model path sends a ping and returns `true` when the daemon
responds with `PONG`, or `false` when the check fails.

The check uses the connection settings documented in `mod_clamav`: a Unix socket
with TCP fallback by default, or socket-only or TCP-only access when configured.
It checks connectivity; it does not scan a file or check virus database freshness.

Access requires permission to use `mod_admin`; other callers receive
`{error, eacces}`. Unknown paths return `{error, unknown_path}`.
The admin dashboard loads this check asynchronously on each page load.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/is_available` | Return a boolean indicating whether the virus scanner responds. Requires `use` permission on `mod_admin`. |

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
