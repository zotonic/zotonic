---
name: zotonic-javascript
description: Use when creating, refactoring, or reviewing Zotonic JavaScript, template JavaScript tags, wires, actions, Erlang event/2 browser handlers, Cotonic workers/models, MQTT client-server communication, authentication workers, and do_ widgets in Zotonic sites or modules.
---

# Zotonic JavaScript

## First Pass

- Inspect nearby templates, JavaScript modules, workers, actions, and Erlang `event/2` handlers before editing; preserve local patterns.
- Prefer Zotonic declarative behavior (`{% wire %}`, actions, Cotonic data attributes, workers, and `do_...` widgets) over one-off DOM scripts.
- Put reusable JavaScript under the module or site `priv/lib/js` tree and include it with `{% lib %}` from the relevant include template.
- Use plain JavaScript and small functional helpers; keep jQuery usage only where existing Zotonic widgets/actions require it.
- For source documentation, Erlang actions, scomps, models, and modules should have `-moduledoc`; use those docs as the local source of truth.

## Template JavaScript

- Include JavaScript libraries with `{% lib "js/file.js" %}` or multi-file `{% lib %}` blocks. Options include `minify`, `nocache`, `async`, and `defer`; `{% lib ... minify %}` can force minification.
- Add module/site JS includes through local include templates such as `_js_include.tpl`, `_admin_js_include.tpl`, `_html_head.tpl`, or `_html_body.tpl` instead of duplicating script tags in pages.
- Put page-specific inline JavaScript inside `{% javascript %}...{% endjavascript %}`. It runs after jQuery is initialized; for dynamic content it runs after the DOM update that inserted the template.
- Ensure the base template has exactly one `{% script %}`, normally near the end of `<body>`. It emits collected JavaScript from `{% javascript %}`, `{% wire %}`, actions, and related scomps.
- Do not put generated JavaScript after `{% script %}` in a page; it will not be included in that page render.
- `{% script nostartup %}` omits startup code, and `format="html" | "escapejs" | "js"` controls output format. Use these only when the caller expects a nonstandard script output.
- Direct `<script>` tags must carry the CSP nonce: `<script nonce="{{ m.req.csp_nonce }}">`. Prefer `{% javascript %}` or `{% lib %}` when possible because they fit Zotonic's collection/minification flow.

## Wires And Actions

- Use `{% wire %}` to bind browser events to actions and optional server postbacks. The default event is click.

```django
{% wire id="show" action={show target="message"} %}
<button id="show" type="button">{_ Show _}</button>
```

- Use `type="submit"` to wire form submission. The target form should have an id and usually `method="post" action="postback"`.

```django
{% wire id="edit-form" type="submit" postback={save id=id} delegate=`mod_example` %}
<form id="edit-form" method="post" action="postback">
    ...
</form>
```

- A click wire with `postback=...` sends a `#postback{}` to the delegate. A submit wire sends a `#submit{}`.
- Use `delegate=`module`` when the `event/2` handler is not in the controller or current module.
- Use repeated `action={...}` arguments for client-side effects before/after a postback; keep user-visible text translated.
- Named wires can be triggered from JavaScript with `z_event("name")`: `{% wire name="refresh-list" action={update target="list" template="_list.tpl"} %}`.
- MQTT wires can subscribe client actions to topics when `mod_mqtt` is enabled, for example `{% wire type={mqtt topic="~site/public/hello"} action={growl text="hello"} %}`.
- Actions live under `src/actions/` as `action_<module>_<name>.erl`. They normally implement `render_action/4` and should document arguments, generated JavaScript, postbacks, and security assumptions in `-moduledoc`.

## Erlang Event Handlers

- Browser wire events are received by `event/2`; include the relevant records via `zotonic.hrl` or `zotonic_wired.hrl`.
- `#postback{message, trigger, target}` is sent for normal postbacks from clicks and explicit postback actions.
- `#submit{message, form, target}` is sent for wired form submits; access fields with `z_context:get_q/2`, `z_context:get_q_all/1`, or `z_context:get_q_validated/2`.
- `#postback_notify{message, trigger, target, data}` is a notification-style event used by JavaScript postback handlers; see `zotonic_notifications.hrl` and nearby module `event/2` clauses for exact payloads.
- Return the updated `Context`. Use `z_render:update/3`, `replace/3`, `insert_*`, `dialog/4`, `dialog_close/1`, `growl/2`, and `z_render:wire/2` to queue browser responses.

## Client Postback Notify

- Send a `#postback_notify{}` from browser JavaScript with `z_notify(message, params)`, defined in `apps/zotonic_mod_wires/priv/lib/js/apps/zotonic-wired.js`.
- Without `z_delegate`, `z_notify` sends to the server-side `postback_notify` observer chain via the `notify` delegate. With `z_delegate: 'mod_name'`, it calls `mod_name:event(#postback_notify{}, Context)`.
- Use `z_target_id` for the element that should receive possible updates and `z_trigger_id` for the triggering element. Other params are available as request/query values in `Context`.
- `z_notify` automatically sends the current CSP nonce and any stored postback data.

```javascript
z_notify("update", {
    z_delegate: "mod_admin",
    z_target_id: targetId,
    z_trigger_id: triggerId,
    id: resourceId
});
```

```erlang
event(#postback_notify{message = <<"update">>, target = TargetId}, Context) ->
    Id = z_context:get_q(<<"id">>, Context),
    Html = z_template:render("_rsc_item.tpl", [{id, Id}], Context),
    z_render:update(TargetId, Html, Context).
```

## Postback Client State

- Zotonic wires can attach client-side state to every postback, submit, and `postback_notify` sent by `zotonic-wired.js`.
- Page-scoped state is stored as JSON in the `<body data-wired-postback="...">` attribute.
- Tab-scoped state is stored under `sessionStorage.postbackData`; persistent browser/site state is stored under `localStorage.postbackData`.
- `z_postback_data()` merges these three stores and sends the result as a query parameter named `z_postback_data`.
- Merge precedence is body attribute over sessionStorage over localStorage. Use page-scoped state for current-page UI state, sessionStorage for per-tab state, and localStorage only for state that should survive reloads and new tabs.
- `z_notify(...)` adds this data to `#postback_notify{data = #{ q := ... }}`. Normal postback and submit events add the same `z_postback_data` value to the `#postback_event{data = #{ q := ... }}` payload before it becomes `#postback{}` or `#submit{}`.
- On the server, read it with `z_context:get_q(<<"z_postback_data">>, Context)` and validate it like any other client-provided value.

```erlang
case z_context:get_q(<<"z_postback_data">>, Context) of
    #{ <<"z_edit_language">> := Lang } ->
        handle_language(Lang, Context);
    _ ->
        Context
end.
```

- Set page-scoped state from client JavaScript with `z_postback_data_set(Name, Value)`. Read it with `z_postback_data_get(Name)`.
- Set tab-scoped state with `z_postback_data_set_session(Name, Value)`, which publishes to `model/sessionStorage/post/postbackData/<Name>`.
- Set persistent state with `z_postback_data_set_local(Name, Value)`, which publishes to `model/localStorage/post/postbackData/<Name>`.
- Delete a key from all three stores with `z_postback_data_delete(Name)`.

```javascript
z_postback_data_set("z_edit_language", "nl");
z_postback_data_set_session("wizard_step", 3);
z_postback_data_set_local("preferred_panel", "advanced");
z_postback_data_delete("wizard_step");
```

- Set initial page-scoped state from a template by rendering the JSON-encoded map on `<body data-wired-postback="...">`; escape it as an HTML attribute.
- Set or change state from a server response by emitting JavaScript with a `{script}` action, `{% javascript %}` in rendered HTML, or `z_render:add_script/2`. Escape any values placed into generated JavaScript.
- From Erlang code running in a page/client context, persistent client stores can also be updated by publishing to the current client bridge:

```erlang
z_mqtt:publish(
    [<<"~client">>, <<"model">>, <<"sessionStorage">>, <<"post">>, <<"postbackData">>, <<"wizard_step">>],
    3,
    Context),
z_mqtt:publish(
    [<<"~client">>, <<"model">>, <<"localStorage">>, <<"post">>, <<"postbackData">>, <<"preferred_panel">>],
    <<"advanced">>,
    Context).
```

```erlang
-spec event(#submit{} | #postback{}, z:context()) -> z:context().
event(#submit{message = {save, Args}, form = FormId}, Context0) ->
    Title = z_context:get_q(<<"title">>, Context0),
    Context = save_title(Args, Title, Context0),
    z_render:growl(?__("Saved.", Context), z_render:update(FormId, <<>>, Context));
event(#postback{message = refresh, target = TargetId}, Context) ->
    Html = z_template:render("_list.tpl", [], Context),
    z_render:update(TargetId, Html, Context).
```

## Security

- Always use a nonce on direct script tags: `nonce="{{ m.req.csp_nonce }}"`.
- Treat query arguments, form fields, postback payload data, MQTT payloads, and Cotonic data attribute values as untrusted. Validate in `event/2` and server model callbacks.
- Do not interpolate untrusted template values directly into JavaScript. Use JSON/JS escaping filters appropriate to the local code, and prefer passing structured data via data attributes or MQTT payloads.
- Signed postbacks protect the postback command, not arbitrary form/query data. Validate ids through `m_rsc`, ACL checks, or model functions before modifying state.
- Client-side MQTT topics are subject to bridge/server authorization, but handlers must still validate payload shape, ids, and permissions.

## Client Server Communication

- Zotonic uses Cotonic in the browser and MQTT-style messaging between browser and server.
- `_html_head_cotonic.tpl` creates `cotonic.ready`, pre-connects `cotonic.bridgeSocket` to the `mqtt_transport` WebSocket with the `mqtt` subprotocol, and buffers early click/submit data-attribute events.
- `_js_include.tpl` loads `cotonic/cotonic.js`, `js/apps/zotonic-wired.js`, `js/apps/z.widgetmanager.js`, and other base modules. Include `_html_head.tpl`/`_html_head_admin.tpl` and `_js_include.tpl` through the normal base template flow.
- `controller_mqtt_transport.erl` handles MQTT over WebSocket and authenticated HTTP fallback/post traffic. Authentication can use the `z.auth` cookie or MQTT username/password.
- Add connection status HTML with `_bridge_warning.tpl` where the site wants to show “Connecting...” and a connection-test link.
- MQTT topics are slash-separated and support `+` and `#` wildcards. Server `z_mqtt` supports QoS `0`, `1`, and `2`, and options such as `retain`; most browser communication uses QoS 0 unless a call explicitly asks otherwise.
- Do not assume exactly-once delivery for JavaScript relay traffic. The browser/server bridge queues while reconnecting, and the server page process buffers until the browser connects, but persistent semantics depend on the server topic, retain flag, and QoS path being used.
- There are two topic trees: the browser's local Cotonic broker and Zotonic's server broker. `bridge/origin/...` on the client publishes/calls the server origin tree. Server topics under `bridge/<client-id>/...` route to the browser tree.
- Server shorthand topics include `~client` for the current client bridge and `~user` for the current user topic. Core server topic roots include `public`, `test`, `user`, `user/<id>`, and `bridge/<client-id>`.
- Server models are reachable through topics such as `bridge/origin/model/<model>/get/...`, `bridge/origin/model/<model>/post/...`, and `bridge/origin/model/<model>/delete/...`; server-side `mod_mqtt` dispatches them through `z_model:callback/5`.
- Client-routing topics on the server are the `bridge/...` topics; use them for page-specific browser communication, not for durable global state.

## Client Publish Subscribe

- Wait for `cotonic.ready` before browser code depends on Cotonic startup.

```javascript
cotonic.ready.then(() => {
    const sub = cotonic.broker.subscribe("bridge/origin/test/#", (msg, bindings, options) => {
        console.log(msg, bindings, options);
    });

    cotonic.broker.publish("bridge/origin/test/hello", { text: "Hello" });

    cotonic.broker.call(
        "bridge/origin/model/template/get/render/_item.tpl",
        { vars: { id: 123 } },
        { qos: 1 }
    ).then((html) => cotonic.broker.publish("model/ui/replace/item", html));
});
```

- Use `cotonic.broker.publish(topic, payload, options)` for fire-and-forget messages, `subscribe(filter, callback, options)` for subscriptions, and `call(topic, payload, options)` when a response topic is expected.

## Server Publish Subscribe

- Use `z_mqtt` for Erlang-side MQTT. Prefer binary topic segments or the helper mapping functions when topic parts are dynamic.

```erlang
z_mqtt:subscribe([<<"my">>, <<"topic">>, '#'], Context),
z_mqtt:publish([<<"my">>, <<"topic">>], #{status => ok}, #{qos => 1, retain => true}, Context).
```

- A subscribed Erlang process receives `{mqtt_msg, Msg}` when using process subscriptions.
- Modules can export quoted `mqtt:` callback functions. `mod_mqtt` scans active modules and subscribes these with a sudo context.

```erlang
-export(['mqtt:test/#'/2]).

'mqtt:test/#'(#{payload := Payload, topic := Topic}, Context) ->
    handle_test_message(Topic, Payload, Context).
```

## Cotonic

- Cotonic is the browser-side runtime for isolated workers, models, topic routing, and interactive DOM updates. See [cotonic.org](https://cotonic.org/) for the upstream concepts and use local Zotonic sources for Zotonic-specific topics.
- Workers are spawned by Cotonic (`cotonic.spawn`, `cotonic.spawn_named`, or Zotonic template worker tags). Worker code uses `self.subscribe`, `self.publish`, and `self.call` and declares `provides`/`depends` so startup can order services.
- The service worker coordinates cross-tab/browser features. Zotonic uses topics such as `model/serviceWorker/post/broadcast/+channel` and `model/serviceWorker/event/broadcast/+channel` for browser-window synchronization, including auth state sync.
- Common client models include `model/localStorage`, `model/sessionStorage`, `model/sessionId`, `model/document`, `model/location`, `model/window`, `model/ui`, `model/serviceWorker`, `model/lifecycle`, `model/autofocus`, `model/dedup`, `model/auth`, `model/auth-ui`, `model/oauth`, `model/loadmore`, and module-specific models such as `model/fileuploader`.
- Use local/client models directly from JavaScript (`model/localStorage/get/key`) and server models via the origin bridge (`bridge/origin/model/rsc/get/...`). Server code can target client models by publishing to the current client bridge (`~client` or `bridge/<client-id>/...`).
- Cotonic data attributes publish DOM events to topics: `data-onclick-topic`, `data-onsubmit-topic`, `data-oninput-topic`, with matching `data-on...-cancel` attributes for cancellation behavior.
- Add `data-cotonic-pathname-search="{% cotonic_pathname_search %}"` to `<body>` in normal pages so Cotonic location/UI logic has the routed pathname/search value.
- The interactive DOM is updated by publishing to UI topics such as `model/ui/insert/<key>`, `model/ui/update/<key>`, `model/ui/replace/<key>`, `model/ui/delete/<key>`, and `model/ui/render-template/<key>`. Listen for DOM update events when follow-up initialization is needed.
- Check Zotonic Cotonic workers and models under `apps/*/priv/lib/js/**/*.worker.js`, `apps/*/priv/lib/js/models/*.js`, and base files such as `apps/zotonic_mod_wires/priv/lib/js/apps/zotonic-wired.js`.

## Authentication

- `zotonic.auth.worker.js` owns browser auth state. It checks, refreshes, logs on/off, resets, changes, and switches users by calling `/zotonic-auth` and publishing auth model events.
- Important auth topics include `model/auth/post/check`, `model/auth/post/logon`, `model/auth/post/logoff`, `model/auth/post/refresh`, `model/auth/post/form/logon`, `model/auth/post/onetime-token`, `model/auth/event/auth`, `model/auth/event/auth-user-id`, `model/auth/event/auth-error`, and `model/auth/event/ui-status`.
- The `z.auth` cookie is the browser auth cookie managed by server authentication token code and refreshed/reset via `/zotonic-auth`. Client code should go through `model/auth` topics instead of editing this cookie directly.
- `zotonic.auth-ui.worker.js` owns auth UI flows such as login views, reminders, verification messages, reset, change, and confirmation. It listens to `model/auth-ui/post/...` and calls server models via `bridge/origin/model/authentication/...`.
- `zotonic.oauth.worker.js` coordinates OAuth authorize/redirect flows, stores temporary OAuth data through `model/localStorage`/`model/sessionStorage`, calls `bridge/origin/model/oauth2_service/post/oauth-redirect`, and publishes auth onetime-token or UI status topics as needed.

## do Widgets

- `z.widgetmanager.js` initializes classes starting with `do_`. The class `do_clickable` maps to the jQuery widget/plugin `clickable`; `do_dialog` maps to `show_dialog`.
- Widget options are read from metadata/data attributes such as `data-adminwidget='{"minifiedOnInit": true}'`, merged with widget defaults, and passed to the plugin.
- Run widgets by adding the class and including the widget JavaScript through `{% lib %}`. The widget manager initializes existing DOM on page startup and new nodes after IncrementalDOM/Cotonic updates.

```django
{% lib "js/modules/z.clickable.js" %}
<div class="do_clickable" data-clickable='{"url":"/example"}'>...</div>
```

- Define widgets as normal jQuery UI/Zotonic widgets in `priv/lib/js/modules/` and set defaults on the widget, for example `$.ui.clickable.defaults = {...}`.
- Core Zotonic widgets under `apps/` include base widgets `do_clickable`, `do_smiley`, `do_feedback`, `do_timesince`, `do_tooltip`, `do_inputoverlay`, `do_autocomplete`, `do_zeditor`, `do_datepicker`, `do_formdirty`, `do_popupwindow`, `do_filepreview`, `do_forminit`, and `do_dialog`.
- Additional core module widgets include `do_live` (`mod_mqtt`), `do_adminwidget` (`mod_admin`), `do_menuedit`/`do_trash`/Superfish menu behavior (`mod_menu`), `do_cookie_consent`, `do_survey_test_feedback`, `do_gaq_track`, and `do_make_diff`.
- Before adding a new widget, run `rg "do_<name>|\$\.widget|\.defaults" apps/*/priv/lib/js` to avoid duplicating an existing core widget.
