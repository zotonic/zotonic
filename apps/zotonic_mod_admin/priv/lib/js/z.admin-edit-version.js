/* Copyright 2026 Zotonic. Apache-2.0. */
(function () {
    "use strict";
    if (window.z_admin_edit_version) return;
    const forms = new Map();
    let sequence = 0;

    function watch(formId, id) {
        const form = document.getElementById(formId);
        if (!form || forms.get(formId)?.form === form) return;
        forms.get(formId)?.dispose();
        // Overview pages use a container instead of a submit form.
        const input = form.querySelector('input[name="z_edit_version"]');
        if (!input) return;
        const warning = form.querySelector('[data-edit-version-warning]');
        const state = { form, id, observed: Number(input.value), pending: false, dirty: false };
        const options = { wid: 'admin-edit-version-' + (++sequence) };
        const topics = ['bridge/origin/model/rsc/event/' + id + '/update',
            'bridge/origin/model/media/event/' + id + '/+', 'bridge/origin/status',
            'model/admin/event/media-version'];
        let poll, timeout, dismissedVersion = 0;
        const show = () => {
            if (warning) warning.hidden = state.pending || state.observed <= Number(input.value)
                || state.observed <= dismissedVersion;
        };
        state.show = show;
        const check = () => {
            if (!form.isConnected) { state.dispose(); return; }
            const baseline = Number(input.value);
            cotonic.broker.call('bridge/origin/model/rsc/get/' + id + '/version', {}, { timeout: 10000 })
                .then(msg => {
                    // Discard a read started before our own save acknowledgement.
                    if (!form.isConnected || baseline !== Number(input.value)) return;
                    if (msg.payload.status === 'ok' && Number.isInteger(msg.payload.result)) {
                        state.observed = Math.max(state.observed, msg.payload.result);
                        show();
                    }
                }).catch(() => { /* Server-side version checks also protect offline forms. */ });
        };
        const updated = msg => {
            const version = msg.payload?.version;
            // Processing only changes media, not the resource fields being edited.
            // Never skip a missing version: it could contain somebody else's edit.
            if (msg.topic === topics[0] && msg.payload?.reason === 'media_processing'
                && msg.payload.id === id && Number.isInteger(version)
                && msg.payload.previous_version === Number(input.value)
                && version === msg.payload.previous_version + 1) {
                input.value = String(version);
            }
            if (msg.topic === 'model/admin/event/media-version') {
                // A media replacement leaves the form's unsaved text intact.
                if (form.tagName === 'FORM' && !form.hasAttribute('data-translation-edit-form')
                    && msg.payload.id === id && Number.isInteger(version)
                    && msg.payload.previous_version === Number(input.value)
                    && version > msg.payload.previous_version) {
                    input.value = String(version);
                    state.observed = Math.max(state.observed, version);
                    show();
                }
                return;
            }
            if (Number.isInteger(version)) {
                state.observed = Math.max(state.observed, version);
                show();
            } else check();
        };
        const dirty = () => { state.dirty = true; };
        const submit = () => {
            state.pending = true;
            show();
            clearTimeout(timeout);
            timeout = setTimeout(() => { state.pending = false; show(); check(); }, 30000);
        };
        state.finish = version => {
            clearTimeout(timeout);
            state.pending = false;
            if (Number.isInteger(version)) {
                input.value = String(version);
                state.dirty = false;
            }
            show();
            check();
        };
        const reload = () => {
            const button = form.querySelector('[data-edit-version-reload]');
            if (!state.dirty || window.confirm(button.dataset.confirm)) window.location.reload();
        };
        const dismiss = () => {
            dismissedVersion = state.observed;
            show();
        };
        form.addEventListener('input', dirty);
        form.addEventListener('change', dirty);
        form.addEventListener('z:editorChange', dirty);
        window.jQuery?.(form).on('z:editorChange.adminEditVersion', dirty);
        window.jQuery?.(form).on('z:formSubmit.adminEditVersion', submit);
        form.addEventListener('submit', submit, true);
        form.querySelector('[data-edit-version-reload]')?.addEventListener('click', reload);
        form.querySelector('[data-edit-version-dismiss]')?.addEventListener('click', dismiss);
        window.addEventListener('focus', check);
        state.dispose = () => {
            clearInterval(poll);
            clearTimeout(timeout);
            cotonic.broker.unsubscribe(topics, options);
            window.removeEventListener('focus', check);
            form.removeEventListener('input', dirty);
            form.removeEventListener('change', dirty);
            form.removeEventListener('z:editorChange', dirty);
            window.jQuery?.(form).off('.adminEditVersion');
            form.removeEventListener('submit', submit, true);
            form.querySelector('[data-edit-version-reload]')?.removeEventListener('click', reload);
            form.querySelector('[data-edit-version-dismiss]')?.removeEventListener('click', dismiss);
            if (forms.get(formId) === state) forms.delete(formId);
        };
        forms.set(formId, state);
        cotonic.broker.subscribe(topics, updated, options);
        poll = setInterval(check, 15000);
        check();
    }

    window.z_admin_edit_version = {
        watch,
        saved(formId, id, version) {
            const state = forms.get(formId);
            if (state?.id === id && state.pending && state.form.isConnected) state.finish(version);
        },
        savedOverview(id, expectedVersion, version) {
            const expected = Number(expectedVersion);
            if (!Number.isInteger(expected) || !Number.isInteger(version) || version < expected) return;
            for (const state of forms.values()) {
                // A fresh dialog can save while its parent overview is already stale.
                // Only acknowledge saves based on the version displayed by this overview.
                if (state.id === id && state.form.isConnected
                    && state.form.hasAttribute('data-edit-version-overview')
                    && Number(state.form.querySelector('input[name="z_edit_version"]').value) === expected) {
                    state.finish(version);
                }
            }
        }
    };
})();
