/* Capture diagnostics from an explicitly attached page. Never replace console output. */
(() => {
    "use strict";
    if (window.z_development_capture) return;
    window.z_development_capture = { install };

    function install(button, labels, allowConsole) {
        const storageKey = "zotonic.development.capture";
        const panel = document.createElement("div");
        panel.id = "development-capture-panel";
        panel.hidden = true;
        const trace = document.createElement("a");
        trace.href = button.href;
        trace.target = button.target;
        trace.textContent = labels.trace;
        const attach = document.createElement("button");
        attach.type = "button";
        attach.textContent = labels.attach;
        const stop = document.createElement("button");
        stop.type = "button";
        stop.textContent = labels.stop;
        const status = document.createElement("output");
        panel.append(trace, attach, stop, status);
        document.body.append(panel);
        button.addEventListener("click", event => {
            if (event.ctrlKey || event.metaKey || event.shiftKey || event.altKey) return;
            event.preventDefault();
            panel.hidden = !panel.hidden;
        });

        let id;
        try { id = sessionStorage.getItem(storageKey); } catch (_) { /* Storage may be disabled. */ }
        let active = false;
        let entries = [];
        let wrapper;
        let original;
        let busy = false;

        const call = async (method, path, payload = {}) => {
            await cotonic.ready;
            const response = await cotonic.broker.call(
                `bridge/origin/model/development_capture/${method}/${path}`, payload);
            if (response.payload.status !== "ok") throw new Error(response.payload.error || "capture_failed");
            return response.payload.result;
        };
        const save = value => {
            id = value;
            try {
                if (id) sessionStorage.setItem(storageKey, id);
                else sessionStorage.removeItem(storageKey);
            } catch (_) { /* Capture still works without persistence. */ }
        };
        const text = value => {
            try {
                return typeof value === "string" ? value : JSON.stringify(value) ?? String(value);
            } catch (_) { return "[unserializable]"; }
        };
        const setActive = value => {
            active = value;
            if (active && allowConsole && !wrapper) {
                original = console.log;
                wrapper = function(...args) {
                    original.apply(console, args);
                    if (active && entries.length < 50) {
                        entries.push(Array.from(args.map(text).join(" ")).slice(0, 900).join(""));
                    }
                };
                console.log = wrapper;
            } else if ((!active || !allowConsole) && wrapper) {
                if (console.log === wrapper) console.log = original;
                wrapper = undefined;
                entries = [];
            }
        };
        const show = capture => {
            allowConsole = capture.console_allowed === true;
            setActive(capture.state === "active");
            status.textContent = `${capture.id} — ${capture.state}`;
        };
        const fail = error => {
            setActive(false);
            status.textContent = String(error.message || error);
        };
        attach.addEventListener("click", async () => {
            if (busy) return;
            busy = true;
            try {
                if (!id) {
                    const capture = await call("post", "attach", {path: location.pathname});
                    save(capture.id);
                }
                show(await call("post", `start/${id}`));
            } catch (error) { fail(error); save(undefined); }
            finally { busy = false; }
        });
        stop.addEventListener("click", async () => {
            if (!id || busy) return;
            busy = true;
            try {
                // Send the final batch before stopping. No automatic replay of log batches.
                await call("post", `poll/${id}`, {entries: entries.splice(0)});
                show(await call("post", `stop/${id}`));
            } catch (error) { fail(error); }
            finally { busy = false; }
        });
        const poll = async () => {
            if (id && !busy) {
                busy = true;
                try { show(await call("post", `poll/${id}`, {entries: entries.splice(0)})); }
                catch (error) {
                    fail(error);
                    if (String(error.message).includes("enoent")) save(undefined);
                } finally { busy = false; }
            }
            setTimeout(poll, 1000);
        };
        poll();
    }
})();
