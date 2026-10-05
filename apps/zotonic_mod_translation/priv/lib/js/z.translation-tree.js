/* Tree jobs are server-owned. Subscribe before checking status, and poll to recover
 * from missed messages, reconnects and worker failures. Never unlock a stale form. */
(function () {
    "use strict";
    if (window.z_translation_tree) return;
    let root, labels, topic, timer, baselineJob, activeJob, activeRoot, lastState, modal, pending = false;
    const api = "bridge/origin/model/translation_tree/";
    const isDirty = () => document.querySelector('[data-formdirty="true"]') !== null;
    const text = (tag, value) => Object.assign(document.createElement(tag), {textContent: value});

    function progress(state) {
        if (!state) return;
        // A node restart can lose cached job history. The known job has stopped.
        if (state.state === "idle" && activeJob) state = {...lastState, state: "failed"};
        if (!state.job) return;
        // A fast job can finish between polls. Its new job id still makes this form stale.
        if (!activeJob && state.state !== "running" && state.job === baselineJob) return;
        if (activeJob && state.job !== activeJob && state.state !== "running") return;
        if (lastState && state.job === lastState.job) {
            if (lastState.state !== "running" || state.done < lastState.done
                || JSON.stringify(state) === JSON.stringify(lastState)) return;
        }
        lastState = state;
        activeJob = state.job;
        activeRoot = state.root;
        if (!modal || !modal.isConnected) {
            z_dialog_open({title: state.operation === "remove" ? labels.removing : labels.title, text: '<div id="translation-tree-progress" role="status" aria-live="polite"></div>',
                backdrop: "static", keyboard: false, level: 0});
            modal = document.getElementById("translation-tree-progress");
            // Prevent scripts or another dialog from hiding the blocking modal.
            const dialog = $(modal).closest('.modal');
            dialog.on('hide.bs.modal.translationTree', event => event.preventDefault());
            dialog.attr('aria-modal', 'true').attr('role', 'dialog').trigger('focus');
            const blockEditor = () => Array.from(document.body.children).forEach(element => {
                if (!element.contains(modal)) element.inert = true;
            });
            blockEditor();
            new MutationObserver(blockEditor).observe(document.body, {childList: true});
        }
        $(modal).closest('.modal').find('.modal-title').text(
            state.state === "running" ? (state.operation === "remove" ? labels.removing : labels.title) : labels.ready);
        modal.replaceChildren();
        modal.append(text("p", state.operation === "remove" ? labels.removing : labels.title));
        const bar = document.createElement("progress");
        bar.setAttribute("aria-label", state.operation === "remove" ? labels.removing : labels.title);
        bar.max = state.total || 1;
        bar.value = state.done || 0;
        bar.style.width = "100%";
        modal.append(bar, text("p", `${state.done || 0} / ${state.total || 0}`));
        modal.append(text("p", `${labels.skipped}: ${state.skipped || 0} · ${labels.errors}: ${state.failed || 0}`));
        if (state.state === "running") {
            modal.append(text("p", labels.blocked));
        } else {
            clearInterval(timer);
            modal.append(text("p", state.state === "failed" || state.failed ? labels.failed : labels.done));
            const button = text("button", labels.reload);
            button.type = "button";
            button.className = "btn btn-primary";
            button.addEventListener("click", () => window.location.reload());
            modal.append(button);
        }
    }

    function poll() {
        if (!root) return;
        return cotonic.broker.call(api + "get/status/" + (activeRoot || root), {}, {timeout: 10000})
            .then(msg => { if (msg.payload.status === "ok") progress(msg.payload.result); })
            .catch(() => { /* Keep the editor blocked when connectivity is lost. */ });
    }

    function watch(id, messages, initial) {
        labels = messages;
        if (root !== id) baselineJob = initial?.job;
        progress(initial);
        if (root === id) { poll(); return; }
        if (topic) cotonic.broker.unsubscribe(topic);
        clearInterval(timer);
        root = id;
        topic = "bridge/origin/model/rsc/event/" + id + "/translation_tree";
        cotonic.broker.subscribe(topic, () => poll());
        poll();
        timer = setInterval(poll, 2000);
    }

    async function start(id, options) {
        if (pending || activeJob) return;
        if (isDirty()) { window.alert(labels.dirty); return; }
        pending = true;
        document.querySelectorAll('[data-translation-tree-form] button, [data-translation-tree-remove]')
            .forEach(button => button.disabled = true);
        try {
            if (options.method !== "remove") {
                // Refresh the count immediately before confirmation, as the tree may have changed.
                const details = await cotonic.broker.call(api + "get/" + id, {}, {timeout: 60000});
                if (details.payload.status !== "ok") {
                    window.alert(labels.error);
                    return;
                }
                const message = labels.confirmTree.replace("{count}", details.payload.result.total);
                if (!window.confirm(message)) return;
                options = {...options, confirmed: true};
            }
            // The response acknowledges startup only; progress arrives independently.
            const msg = await cotonic.broker.call(api + "post/" + id, options, {timeout: 60000});
            if (msg.payload.status !== "ok") {
                await poll();
                if (!activeJob) window.alert(labels.error);
                return;
            }
            progress(msg.payload.result);
        } catch (_) {
            // A lost reply does not imply that the sidejob failed to start.
            await poll();
            if (!activeJob) window.alert(labels.error);
        } finally {
            pending = false;
            document.querySelectorAll('[data-translation-tree-form] button, [data-translation-tree-remove]')
                .forEach(button => button.disabled = false);
        }
    }

    document.addEventListener("submit", event => {
        const form = event.target.closest("[data-translation-tree-form]");
        if (!form) return;
        event.preventDefault();
        const values = Object.fromEntries(new FormData(form));
        values.overwrite = !!values.overwrite;
        start(form.dataset.translationTreeForm, values);
    });
    document.addEventListener("click", event => {
        if (event.target.closest("#translate-all")) {
            event.preventDefault();
            if (isDirty()) window.alert(labels.dirty);
            else if (!activeJob) z_event("translation-tree-dialog");
            return;
        }
        const button = event.target.closest("[data-translation-tree-remove]");
        if (!button) return;
        event.preventDefault();
        if (window.confirm(labels.confirm)) {
            start(button.dataset.treeId, {method: "remove", language: button.dataset.translationTreeRemove, confirmed: true});
        }
    });
    window.z_translation_tree = {watch};
}());
