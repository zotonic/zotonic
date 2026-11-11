/* Capture diagnostics from an explicitly attached page. Preserve normal console output. */
(() => {
    "use strict";
    if (window.z_development_capture) return;
    const storageKey = "zotonic.development.capture";
    let statusOutput;
    let installed = false;
    let allowConsole = false;
    let allowWrite = false;
    let id;
    try { id = sessionStorage.getItem(storageKey); } catch (_) { /* Storage may be disabled. */ }
    let active = false;
    let entries = [];
    let wrapper;
    let original;
    let pending = 0;
    let queue = Promise.resolve();
    let generation = 0;
    const detached = Symbol("detached");

    const call = async (method, path, payload = {}, expectedGeneration) => {
        const check = () => {
            if (expectedGeneration !== undefined && expectedGeneration !== generation) throw detached;
        };
        await cotonic.ready;
        check();
        const response = await cotonic.broker.call(
            `bridge/origin/model/development_capture/${method}/${path}`, payload);
        check();
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
        allowWrite = capture.write_allowed === true;
        allowConsole = allowWrite && capture.console_allowed === true;
        setActive(capture.state === "active");
        if (statusOutput) statusOutput.textContent = `${capture.id} — ${capture.state}`;
        return capture;
    };
    const fail = error => {
        setActive(false);
        if (statusOutput) statusOutput.textContent = String(error.message || error);
    };

    // Serialize browser commands and polling so concurrent attachments share one ID.
    const run = (operation, reportFailure = true) => {
        const currentGeneration = generation;
        const request = (method, path, payload) => call(method, path, payload, currentGeneration);
        pending++;
        const result = queue.then(() => currentGeneration === generation ? operation(request) : null).catch(error => {
            if (currentGeneration !== generation) return null;
            if (reportFailure) fail(error);
            throw error;
        }).finally(() => {
            if (currentGeneration === generation) pending--;
        });
        queue = result.catch(() => {});
        return result;
    };
    const missing = error => error.code === "enoent";
    const forget = () => {
        save(undefined);
        setActive(false);
        entries = [];
        allowConsole = false;
        allowWrite = false;
        if (statusOutput) statusOutput.textContent = "";
        return null;
    };
    /** Detach locally, even offline; ignore replies to older attachment commands. */
    const detach = async () => {
        generation++;
        queue = Promise.resolve();
        pending = 0;
        return forget();
    };
    const pollCurrent = async (entries, request) => {
        if (!id) return null;
        try {
            return show(await request("post", `poll/${id}`, {entries}));
        } catch (error) {
            if (missing(error)) return forget();
            throw error;
        }
    };

    const validateId = captureId => {
        if (typeof captureId !== "string" || !captureId || /[/+#\0]/.test(captureId)) {
            throw new TypeError("Expected a capture ID containing a single MQTT topic segment");
        }
    };

    /** Reconnect a session-owned capture, or reuse/create the page's attachment. */
    const attach = captureId => run(async request => {
        if (captureId !== undefined) {
            validateId(captureId);
            // Poll checks write permission and session ownership. Never send the
            // current attachment's buffer to a proposed replacement capture.
            const current = await request("post", `poll/${captureId}`, {entries: []});
            const capture = current.state === "active"
                ? current
                : await request("post", `start/${captureId}`);
            if (id !== captureId) {
                setActive(false);
                entries = [];
            }
            save(captureId);
            return show(capture);
        }
        const current = await pollCurrent(entries.splice(0), request);
        if (current?.state === "active") return current;
        if (!current) {
            const capture = await request("post", "attach", {path: location.pathname});
            save(capture.id);
        }
        return show(await request("post", `start/${id}`));
    }, captureId === undefined);

    /** Return the current server state, or null when unattached/expired. */
    const readStatus = async request => {
        if (!id) return null;
        try {
            return show(await request("get", `status/${id}`));
        } catch (error) {
            if (missing(error)) return forget();
            throw error;
        }
    };
    /** List available site captures without attaching or changing local state. */
    const list = () => call("get", "list");

    // Explicit-ID commands do not adopt the target as the page attachment.
    const captureCall = async (method, kind, captureId) => {
        validateId(captureId);
        try {
            return await call(method, `${kind}/${captureId}`);
        } catch (error) {
            if (missing(error)) return null;
            throw error;
        }
    };
    const readCapture = (kind, captureId) => captureCall("get", kind, captureId);
    const status = captureId => captureId === undefined
        ? run(readStatus)
        : readCapture("status", captureId);

    /** Start an existing capture; server authorization requires a writable admin. */
    const start = captureId => captureCall("post", "start", captureId);

    /** Submit this page's pending logs without starting or stopping its capture. */
    const flush = () => run(async request => {
        const current = await readStatus(request);
        if (!current) return null;
        if (!allowWrite) {
            const error = new Error("eacces");
            error.code = "eacces";
            throw error;
        }
        if (current.state !== "active") return current;
        return pollCurrent(entries.splice(0), request);
    });

    /** Explicit IDs stop without flushing; omit the ID to flush this page first. */
    const stop = captureId => captureId !== undefined
        ? captureCall("post", "stop", captureId)
        : run(async request => {
        if (!await pollCurrent(entries.splice(0), request)) return null;
        return show(await request("post", `stop/${id}`));
    });

    // Reads never submit browser logs or require permission to mutate a capture.
    const evidence = kind => run(async request => {
        if (!id) return null;
        try {
            return await request("get", `${kind}/${id}`);
        } catch (error) {
            if (missing(error)) return forget();
            throw error;
        }
    });

    /** Read the explicit ID or current attachment; return null when missing/expired. */
    const trace = captureId => captureId === undefined
        ? evidence("trace")
        : readCapture("trace", captureId);

    /** Return stored {entries, truncated}; use flush() first to submit pending logs. */
    const logs = captureId => captureId === undefined
        ? evidence("console")
        : readCapture("console", captureId);

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
            await run(async request => {
                // Refresh permissions before collecting or submitting browser logs.
                if (await readStatus(request) && allowWrite) {
                    await pollCurrent(entries.splice(0), request);
                }
            }).catch(() => {});
        }
        setTimeout(poll, 1000);
    };
    window.z_development_capture = {install, attach, detach, list, status, start, stop, flush, trace, console: logs};
    poll();
})();
