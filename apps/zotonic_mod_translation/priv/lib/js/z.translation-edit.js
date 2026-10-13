/* Copyright 2026 Zotonic. Apache-2.0. */
(function () {
    "use strict";
    if (window.z_translation_edit_installed) return;
    window.z_translation_edit_installed = true;
    const pending = new WeakSet();

    function read(field) {
        window.z_editor?.save($(field));
        return field.value;
    }

    function replace(field, text) {
        if (field.disabled || field.readOnly) return false;
        const tiptap = window.z_editor_tiptap?.get(field.id);
        const tinymce = window.tinymce?.get(field.id);
        if (tiptap) {
            if (!window.z_editor_tiptap.setContent(field.id, text)) return false;
            window.z_editor_tiptap.save(field);
        } else if (tinymce) {
            tinymce.undoManager.transact(() => tinymce.setContent(text));
            tinymce.save();
        } else {
            field.value = text;
        }
        field.dispatchEvent(new Event('input', { bubbles: true }));
        field.dispatchEvent(new Event('change', { bubbles: true }));
        return true;
    }

    async function translate(control) {
        if (pending.has(control)) return;
        const field = document.getElementById(control.dataset.field);
        if (!field || field.disabled || field.readOnly) return;
        const original = read(field);
        if (original.trim() && !window.confirm(control.dataset.confirm)) return;
        const select = control.querySelector('[data-translation-language]');
        const button = control.querySelector('[data-translation-start]');
        const status = control.querySelector('[data-translation-status]');
        const submitters = [...control.closest('form').querySelectorAll('[type="submit"]')]
            .map(button => [button, button.disabled]);
        pending.add(control);
        button.disabled = select.disabled = true;
        submitters.forEach(([button]) => { button.disabled = true; });
        control.setAttribute('aria-busy', 'true');
        status.textContent = control.dataset.busy;
        try {
            const msg = await cotonic.broker.call('bridge/origin/model/translation/get/translate_field', {
                id: Number(control.dataset.id), property: control.dataset.property,
                block: control.dataset.block, from: select.value, to: control.dataset.language
            }, { timeout: 120000 });
            if (!control.isConnected || !field.isConnected) return;
            if (read(field) !== original) {
                status.textContent = control.dataset.changed;
            } else if (msg.payload?.status === 'ok' && typeof msg.payload.result === 'string'
                && replace(field, msg.payload.result)) {
                status.textContent = '';
            } else {
                status.textContent = control.dataset.error;
            }
        } catch (_) {
            if (control.isConnected) status.textContent = control.dataset.error;
        } finally {
            pending.delete(control);
            button.disabled = select.disabled = false;
            submitters.forEach(([button, disabled]) => { button.disabled = disabled; });
            control.removeAttribute('aria-busy');
        }
    }
    document.addEventListener('keydown', event => {
        if (event.target.matches('.translation-edit-text') && ['Enter', ' '].includes(event.key)) {
            event.preventDefault();
            event.target.click();
        }
    });
    document.addEventListener('click', event => {
        const button = event.target.closest('[data-translation-start]');
        if (button) translate(button.closest('[data-translation-source]'));
        // Preview links belong to the editable cell, not navigation away from the overview.
        if (event.target.closest('.translation-edit-text a')) event.preventDefault();
    });
    $(document).on('hide.bs.modal', '.modal', function (event) {
        if (event.target === this && this.querySelector('[data-translation-edit-form]')) {
            window.z_editor?.remove($(this));
        }
    });
})();
