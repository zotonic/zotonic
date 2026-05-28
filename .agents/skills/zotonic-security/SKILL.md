---
name: zotonic-security
description: Use when reviewing or implementing security-sensitive Zotonic code in templates, JavaScript, Erlang, models, controllers, cookies, HTTP headers, embeds, client-stored state, access control, CSP, or client/server communication.
---

# Zotonic Security

## First Pass

- Treat every browser-originated value as untrusted: `q`, form fields, postback args, `z_postback_data`, Cotonic/MQTT payloads and topic paths, cookies, URL path segments, uploads, and externally fetched data.
- Security checks belong at the server boundary. Client-side checks improve UX only; they do not authorize anything.
- Prefer existing Zotonic helpers: `m_rsc`, `z_acl`, `z_context:get_q_validated/2`, `z_sanitize`, `z_media_sanitize`, `z_crypto`, `z_context:set_cookie/4`, and `z_context:site_url/2`.
- Read local `-moduledoc` and nearby core code before changing security behavior. Relevant sources include `z_context.erl`, `z_crypto.erl`, `z_model.erl`, `zotonic_notifications.hrl`, `zotonic_wired.hrl`, and module-specific `m_*.erl` files.

## Template Security

- Escape at trust boundaries. Template output is HTML unless proven otherwise.
- `m.rsc` properties are generally sanitized/pre-escaped and safe for direct display, for example `{{ id.title }}` and `{{ m.rsc[id].summary }}`.
- Do not assume every resource property is safe. Values documented as unsafe, commonly named with `_unsafe`, must be escaped or sanitized before output.
- Output from non-resource models is not guaranteed safe. Escape it unless that model explicitly documents safe HTML.
- Values from `q` are always unsafe. They can be strings, booleans, structured data, uploads, or attacker-controlled shapes.
- Never bind an unsafe query id directly as trusted `id`; sanitize through `m.rsc`, for example `{% with m.rsc[q.id].id as id %}`.
- Use escaping/sanitizing filters such as `escape`, `escapejs`, `escapejson`, `sanitize_html`, `sanitize_url`, and `urlencode`.
- Templates are not always rendered from the page you expect. The `m_template` model can render templates through `model/template/get/render/...` with externally supplied payload values that are added to `q`.
- Because templates can be rendered through the template model, do not assume that a partial is executed only in an admin/controller-secured context. If a template exposes sensitive data or performs privileged UI decisions, check ACL in the template or require the calling model/controller to pass sanitized, authorized data.
- Treat include/cache options such as `sudo`, `anondo`, `max_age`, `vary`, and `runtime` as security-relevant because they affect ACL context and cache reuse.
- Direct `<script>` and `<style>` tags must include `nonce="{{ m.req.csp_nonce }}"`; prefer `{% javascript %}` and `{% lib %}` for scripts.

## JavaScript Security

- Never trust values only because they came from Zotonic wires or Cotonic. Validate again in Erlang `event/2`, model callbacks, or controllers.
- Signed postbacks protect the postback command and delegate, not arbitrary form fields, query args, or `z_postback_data`.
- `z_notify(...)`, postbacks, and submits can attach `z_postback_data` from `body[data-wired-postback]`, `sessionStorage.postbackData`, and `localStorage.postbackData`. Treat it as user-controlled input on the server.
- Escape template values inserted into JavaScript with the appropriate JSON/JS escaping. Prefer passing structured JSON via safe filters or data attributes.
- Direct script tags need the CSP nonce. If a script creates another script tag, propagate the nonce explicitly.
- Cotonic data attributes such as `data-onclick-topic`, `data-onsubmit-topic`, and `data-oninput-topic` publish to topics; do not encode secrets or unvalidated ids in topic fragments.
- MQTT/Cotonic payloads and topic path segments must be treated like HTTP request bodies and URL paths: unsafe, attacker-controlled, and requiring validation. Topic ACLs are not payload validation.
- Do not edit auth cookies from JavaScript. Use `model/auth` topics and the authentication workers.

## Erlang Security

- Use binary query keys in Zotonic 1.x, for example `z_context:get_q(<<"id">>, Context)`.
- Never create new atoms from external content. Do not use `binary_to_atom/2`, `list_to_atom/1`, or unconstrained conversions on `q`, JSON, MQTT, filenames, headers, cookies, or uploaded data; use binaries or map to an existing allowlisted atom.
- Prefer `z_context:get_q_validated/2` when a validator exists. Use `z_context:get_q_all_noz/1` or `z_context:get_q_map_noz/1` to drop Zotonic internal args before processing ordinary form data.
- Validate ids with `m_rsc:rid/2` and authorize with `z_acl:is_allowed/3`, `m_rsc:is_visible/2`, or resource APIs that perform ACL checks.
- Use `z_sanitize:uri/1` or `z_context:site_url/2` before redirects or storing URLs. `site_url/2` forces non-site URLs back to the site homepage.
- Use `z_sanitize:html/2`, `z_sanitize:escape_link/2`, `z_string:sanitize_utf8/1`, and `z_media_sanitize` for HTML, text, uploads, SVG, CSV, and imported media.
- `z_sanitize:html/1,2` is the context-aware HTML sanitizer. It parses HTML, removes disallowed elements/attributes, sanitizes URLs, removes risky comments and class/style content, adds safe link behavior, and applies Zotonic embed handling.
- With a `Context`, `z_sanitize:html/2` allows configured extra elements/attributes (`site.html_elt_extra`, `site.html_attr_extra`) and runs sanitizer notifications, including `#sanitize_element{}` and embed URL allowlisting.
- Use `z_sanitize:html/2` for user-supplied rich text, imported HTML, oEmbed body HTML, and uploaded HTML media; do not use plain HTML escaping when the intent is to keep safe markup.
- Use `z_sanitize:escape_props/2` before storing externally supplied resource properties. It escapes/sanitizes values based on property names.
- Use `z_sanitize:escape_props_check/2` when values may already be escaped and should not be double-escaped. This is common for imports or update paths that receive mixed existing/new data.
- `escape_props` and `escape_props_check` inspect property names to choose sanitization:
- `body*` and `*_html` are sanitized as HTML.
- `summary` is escaped and converted to line-break HTML.
- `website`, `@id`, `*_uri`, and `*_url` are URI-sanitized and escaped.
- `blocks` are recursively sanitized as nested property maps/lists.
- `is_a*` and `*_list` are sanitized as lists; `is_*` values become booleans.
- `*_int` values are converted to integers; invalid values become `undefined`.
- `*_id` values are converted to integers where possible; empty ids become `undefined`.
- `*_unsafe` is intentionally not escaped. Use this suffix only when the value is already trusted and documented.
- Unknown scalar properties are HTML-escaped; unknown maps/lists are recursively sanitized.
- Avoid `*_no_acl` functions unless the caller has already performed an explicit ACL check and the code comment makes that clear.
- Avoid `z_acl:sudo/1` and `sudo` template rendering unless absolutely required; keep the sudo scope small and never mix sudo-rendered output into shared caches without a safe `vary`.
- Log security failures with structured `?LOG_*` maps, but do not log tokens, cookies, passwords, full auth headers, or signed payloads.
- When storing client-roundtripped state, protect integrity with `z_crypto` and still check ACL after decoding.

```erlang
SafeHtml = z_sanitize:html(UserHtml, Context),
SafeProps = z_sanitize:escape_props(ExternalProps, Context),
SafeMixedProps = z_sanitize:escape_props_check(ImportedProps, Context).
```

## Model Security

- All model callback functions exposed as `m_get/3`, `m_post/3`, and `m_delete/3` MUST perform access control for every path that returns data or changes state.
- Models are reachable from templates through `m.*` and from the browser/server topic tree through `model/<model>/get|post|delete/...`, including `bridge/origin/model/...` from Cotonic.
- Do not rely on the caller being a trusted template, admin page, or internal worker. Check ACL inside the model callback or call lower-level APIs that already enforce ACL.
- For read paths, filter invisible resources and avoid leaking existence through detailed errors.
- For write/delete paths, check the concrete operation (`insert`, `update`, `delete`, `link`, custom action) and the specific resource/category/content group.
- Payloads passed with the template `::` operator or via MQTT payload/path segments are external input unless the caller is proven internal. Validate type, shape, length, path segments, and ids.
- Document model ACL behavior in `-moduledoc`, including which paths are public, authenticated, admin-only, or resource-ACL based.

```erlang
m_post([<<"publish">>, IdBin], #{payload := Payload}, Context) ->
    case m_rsc:rid(IdBin, Context) of
        undefined -> {error, enoent};
        Id ->
            case z_acl:is_allowed(update, Id, Context) of
                true -> publish_resource(Id, Payload, Context);
                false -> {error, eacces}
            end
    end.
```

## HTTP Security

- Use Zotonic controllers and context helpers instead of raw Cowboy response handling where possible.
- `z_context:set_security_headers/1` installs the default security headers early in request handling. Defaults include `content-security-policy`, `x-content-type-options: nosniff`, `x-permitted-cross-domain-policies: none`, `referrer-policy: strict-origin-when-cross-origin`, and `x-frame-options: SAMEORIGIN` when `frame-ancestors` is `'self'`.
- For normal HTTP requests, `z_cowmachine_middleware` always calls `z_context:set_csp_nonce/1` and then `z_context:set_security_headers/1` after the site, dispatch rule, controller, request data, and context are initialized, before Cowmachine controller callbacks run.
- Do not manually call `set_csp_nonce/1` or `set_security_headers/1` in ordinary controllers/templates. Use the nonce from `m.req.csp_nonce` in templates or `z_context:csp_nonce(Context)` in Erlang, and extend headers through notifications.
- Default CSP includes `default-src 'self'`, nonce-based scripts, `object-src 'none'`, `form-action 'self'`, `worker-src 'self' blob:`, `connect-src 'self' https: wss:`, and CSP reporting to the site endpoint.
- Extend CSP with the `#content_security_header{}` fold notification. Modify whole HTTP security headers with the `#security_headers{}` first notification. Both are defined in `zotonic_notifications.hrl`; all header names are lowercase.
- Use `#cors_headers{}` only for deliberate CORS exposure. Never reflect arbitrary origins with credentials.
- Use `z_context:set_nocache_headers/1` for sensitive/private responses and `z_context:set_noindex_header/1` for pages that must not be indexed.
- Use `z_context:set_resource_headers/2` for resource pages so modules can add resource-specific headers via `#resource_headers{}`.
- For redirects, sanitize first with `z_sanitize:uri/1` or constrain to local URLs with `z_context:site_url/2`.
- For file downloads, set a safe `content-disposition` filename and verify MIME/content restrictions.
- For external HTTP fetches, sanitize URLs, apply allowlists when possible, and consider SSRF risk. URL sanitization alone does not prove a remote URL is safe to fetch.

## File Security

- Assume all uploaded, imported, fetched, or otherwise user-controlled files are malicious, regardless of extension, reported content type, or filename.
- Identify files with `z_media_identify:identify/2`, `z_media_identify:identify/3`, or `z_media_identify:identify_file/2,3` before storing, previewing, converting, or serving them. Use the returned media info, not the browser-provided MIME type, as the basis for decisions.
- Run media/file sanitizer paths where applicable, for example `z_media_sanitize:sanitize/2` for uploaded SVG/HTML/CSV media and `z_media_sanitize:is_file_acceptable/2` before handing files to processors such as image/video converters.
- Use `z_media_identify:extension/2,3` or media APIs for derived filenames/extensions; do not trust or reuse user-provided extensions directly.

## Cookie Security

- Always set cookies through `z_context:set_cookie/3` or `z_context:set_cookie/4`. It applies the site cookie domain when configured and forces `{secure, true}`.
- Set `{http_only, true}` for cookies not intentionally read by JavaScript.
- Set `{same_site, strict}` for authentication/session cookies when compatible; use `{same_site, lax}` for OAuth/state flows that must survive top-level redirects.
- Core authentication cookies:
- `z.auth`: authentication cookie, set with `path=/`, `http_only=true`, `secure=true`, `same_site=strict`; signed/encrypted by authentication token code and replay-token aware.
- `z.autologon`: remember-me cookie, set with `max_age` from autologon expiry, `path=/`, `http_only=true`, `secure=true`, `same_site=strict`.
- `z.state`: state cookie used for OAuth/CSRF-style exchange state, set with `path=/`, `http_only=true`, `secure=true`, `same_site=lax` and reset with `max_age=0`.
- Do not store secrets in JavaScript-readable cookies. If JavaScript must read a preference, keep it non-secret and validate it server-side if it is sent back.
- Reset cookies with the same path/domain/security options used to set them, plus `max_age=0`.

## Client Stored Data And z_crypto

- Use `z_crypto:pickle/2` for URL-safe, signed Erlang terms stored on the client. Decode with `z_crypto:depickle/2`; invalid data raises `checksum_invalid`.
- Use `z_crypto:encode_value/2` for signed/checksummed values commonly stored in cookies. Use `encode_value_expire/3` and `decode_value_expire/2` when the value must expire.
- `z_crypto:checksum/2` and `checksum_assert/3` are useful when signing simple external data or callback parameters.
- `z_crypto` protects integrity, not confidentiality. Base64/base64url output must be treated as readable by the client. Do not store passwords, access tokens, or private data client-side unless a separate confidentiality mechanism is used.
- Always handle decode failures and expired values as normal invalid input; do not crash request handling on attacker-supplied values.

```erlang
Token = z_crypto:pickle(#{id => Id, action => confirm}, Context),
case catch z_crypto:depickle(TokenFromClient, Context) of
    #{id := Id, action := confirm} -> continue(Id, Context);
    _ -> {error, checksum_invalid}
end.
```

## Embed And External Content Security

- Do not store or render arbitrary supplied iframe/embed HTML.
- For video embeds, extract and store a supported service and id, then render fresh iframe HTML. Core video embed code supports YouTube and Vimeo, URL-encodes the id, and uses `z_sanitize:default_sandbox_attr(Context)` on iframes.
- For oEmbed, sanitize provider URLs with `z_sanitize:uri/1` and sanitize URL fields in returned JSON before storage or rendering.
- Preserve cookie/privacy wrappers for external content. The `#wrap_embed_html{}` notification lets modules wrap external HTML so user consent settings can be honored.
- Treat preview URLs and provider metadata as external input. Sanitize URLs and avoid trusting titles, authors, dimensions, or HTML from providers without the existing sanitizer/wrapper path.

## Review Checklist

- Are all model `m_get`, `m_post`, and `m_delete` paths authorized?
- Are all `q`, form, MQTT payloads/topic paths, postback, and cookie values validated before use?
- Are non-`m.rsc` template outputs escaped unless documented safe?
- Since templates can always be rendered with external `q`, does the template still avoid leaks/actions when all `q` values are attacker-controlled?
- Are direct scripts/styles nonce-protected?
- Are redirects and external URLs sanitized and constrained?
- For local redirects from external or untrusted input, is `z_context:is_site_url/2` or `z_context:site_url/2` used to prevent open redirects?
- Are redirect paths/URLs sanitized with `z_sanitize:uri/1` before they are used or stored?
- Are cookies `secure`, `http_only` when possible, and using appropriate `same_site`?
- Is client-stored state signed with `z_crypto` if integrity matters, and is no secret stored client-readable?
- Are embed/oEmbed URLs and generated iframe attributes sanitized and sandboxed?
