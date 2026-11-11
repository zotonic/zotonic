/* Capture diagnostics from an explicitly attached page. Preserve normal console output. */
(() => {
    "use strict";
    if (window.z_development_capture) return;
    const storageKey = "zotonic.development.capture";
    let statusOutput;
    let installed = false;
    let allowConsole = false;
    let id;
    try { id = sessionStorage.getItem(storageKey); } catch (_) { /* Storage may be disabled. */ }
    let active = false;
    let entries = [];
    let wrapper;
    let original;
    let pending = 0;
    let queue = Promise.resolve();

    const call = async (method, path, payload = {}) => {
        await cotonic.ready;
        const response = await cotonic.broker.call(
            `bridge/origin/model/development_capture/${method}/${path}`, payload);
        if (response.payload.status !== "ok") {
            const code = response.payload.error || "capture_failed";
            const error = new Error(typeof code === "string" ? code : JSON.stringify(code));
            error.code = code;
            throw error;
        }
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
        if (statusOutput) statusOutput.textContent = `${capture.id} — ${capture.state}`;
        return capture;
    };
    const fail = error => {
        setActive(false);
        if (statusOutput) statusOutput.textContent = String(error.message || error);
    };

    // Serialize browser commands and polling so concurrent attachments share one ID.
    const run = operation => {
        pending++;
        const result = queue.then(operation).catch(error => {
            fail(error);
            throw error;
        }).finally(() => { pending--; });
        queue = result.catch(() => {});
        return result;
    };
    const missing = error => error.code === "enoent";
    const forget = () => {
        save(undefined);
        setActive(false);
        if (statusOutput) statusOutput.textContent = "";
        return null;
    };
    const pollCurrent = async entries => {
        if (!id) return null;
        try {
            return show(await call("post", `poll/${id}`, {entries}));
        } catch (error) {
            if (missing(error)) return forget();
            throw error;
        }
    };

    /** Reuse a valid attachment, or create one, and start collecting. */
    const attach = () => run(async () => {
        const current = await pollCurrent(entries.splice(0));
        if (current?.state === "active") return current;
        if (!current) {
            const capture = await call("post", "attach", {path: location.pathname});
            save(capture.id);
        }
        return show(await call("post", `start/${id}`));
    });

    /** Return the current server state, or null when unattached/expired. */
    const status = () => run(() => pollCurrent([]));

    /** Flush the browser buffer, then stop; retain the ID and server evidence. */
    const stop = () => run(async () => {
        if (!await pollCurrent(entries.splice(0))) return null;
        return show(await call("post", `stop/${id}`));
    });

    function install(button, labels) {
        if (installed) return;
        installed = true;
        const panel = document.createElement("div");
        panel.id = "development-capture-panel";
        panel.hidden = true;
        const trace = document.createElement("a");
        trace.href = button.href;
        trace.target = button.target;
        trace.textContent = labels.trace;
        const attachButton = document.createElement("button");
        attachButton.type = "button";
        attachButton.textContent = labels.attach;
        const stopButton = document.createElement("button");
        stopButton.type = "button";
        stopButton.textContent = labels.stop;
        statusOutput = document.createElement("output");
        panel.append(trace, attachButton, stopButton, statusOutput);
        document.body.append(panel);
        button.addEventListener("click", event => {
            if (event.ctrlKey || event.metaKey || event.shiftKey || event.altKey) return;
            event.preventDefault();
            panel.hidden = !panel.hidden;
        });

        // Public methods own state and error reporting; UI handlers consume rejections.
        attachButton.addEventListener("click", () => window.z_development_capture.attach().catch(() => {}));
        stopButton.addEventListener("click", () => window.z_development_capture.stop().catch(() => {}));
        window.z_development_capture.status().catch(() => {});
    }

    const poll = async () => {
        if (id && pending === 0) {
            await run(() => pollCurrent(entries.splice(0))).catch(() => {});
        }
        setTimeout(poll, 1000);
    };
    window.z_development_capture = {install, attach, status, stop};
    poll();
})();
