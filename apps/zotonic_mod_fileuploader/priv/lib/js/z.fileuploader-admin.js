/* Contextual admin file drops. Transport remains in the fileuploader worker.
 * Dialog zones only populate the native input; their existing postback owns submission.
 */
(function() {
    'use strict';

    const selector = '[data-fileuploader], [data-fileuploader-input]';
    let highlighted;
    let dragDepth = 0;
    const pending = new Map();
    let uploaderBusy = false;
    const batches = new Map();
    let attempt = 0;
    const message = key => document.getElementById('fileuploader-messages')?.dataset[key] || '';
    const uploadError = error => ['eacces', 'file_not_allowed'].includes(error) ? message('notAllowed')
        : error === 'file_too_large' ? message('tooLarge') : '';
    const containsFiles = event => Array.from(event.dataTransfer?.types || []).includes('Files');

    function reset() {
        highlighted?.classList.remove('is-dragover', 'dragover');
        highlighted = undefined;
        dragDepth = 0;
    }

    function zoneFor(target) {
        const zone = target.closest?.(selector);
        const modal = Array.from(document.querySelectorAll('.modal')).reverse().find(el => el.getClientRects().length);
        // A dialog owns the interaction, even when it has no upload field.
        if (!zone || (modal && !modal.contains(zone))) return null;
        return zone;
    }

    function report(zone, text, state = 'info') {
        const status = zone.querySelector('.fileuploader-status');
        if (status) {
            status.textContent = text;
            ['info', 'uploading', 'success', 'error'].forEach(value =>
                status.classList.toggle('is-' + value, value === state));
        } else if (state === 'error') {
            window.alert(text);
        }
    }

    function validationError(zone, files) {
        const input = zone.querySelector('input[type="file"]');
        const options = JSON.parse(zone.getAttribute('data-fileuploader') || '{}');
        const single = zone.hasAttribute('data-fileuploader-single') || options.is_medium
            || (zone.hasAttribute('data-fileuploader-input') && !input?.multiple);
        if (single && files.length !== 1) return message('single');
        const maxSize = Number(document.getElementById('fileuploader-messages')?.dataset.maxSize);
        const invalid = files.find(file => file.size === 0 || (maxSize > 0 && file.size > maxSize)
            || !accepts(file, input?.accept || zone.dataset.fileuploaderAccept));
        if (!invalid) return '';
        const key = invalid.size === 0 ? 'empty' : maxSize > 0 && invalid.size > maxSize ? 'oversized' : 'accept';
        return message(key).replace('{filename}', invalid.name)
            .replace('{size}', maxSize >= 1048576
                ? (maxSize / 1048576).toLocaleString(undefined, {maximumFractionDigits: 2}) + ' MB'
                : maxSize >= 1024 ? (maxSize / 1024).toLocaleString(undefined, {maximumFractionDigits: 2}) + ' KB'
                : maxSize + ' ' + message('bytes'));
    }

    function accepts(file, accept) {
        return !accept || accept.split(',').some(value => {
            const type = value.trim().toLowerCase();
            if (!type) return false;
            if (type.startsWith('.')) return file.name.toLowerCase().endsWith(type);
            // Missing browser MIME metadata is not evidence of an invalid file.
            if (!file.type) return true;
            return type.endsWith('/*') ? file.type.toLowerCase().startsWith(type.slice(0, -1))
                : file.type.toLowerCase() === type;
        });
    }

    async function selectFiles(zone, files, dropped) {
        if (zone.getAttribute('aria-busy') === 'true' || !files.length) return;
        const input = zone.querySelector('input[type="file"]');
        const options = JSON.parse(zone.getAttribute('data-fileuploader') || '{}');
        const error = validationError(zone, files);
        if (error) {
            zone.querySelector('.fileuploader-results')?.remove();
            zone.querySelector('.fileuploader-status')?.classList.toggle('has-results', false);
            report(zone, error, 'error');
            if (!dropped && input) input.value = '';
            return;
        }
        if (zone.hasAttribute('data-fileuploader-input')) {
            if (dropped) {
                const transfer = new DataTransfer();
                files.forEach(file => transfer.items.add(file));
                input.files = transfer.files;
                input.dispatchEvent(new Event('change', { bubbles: true }));
            }
            const form = input.form;
            if (form && batches.has(form.id)) {
                batches.delete(form.id);
                form.querySelector('.fileuploader-results')?.remove();
                form.querySelector('.fileuploader-status')?.classList.toggle('has-results', false);
            }
            report(zone, message('selected'));
            return;
        }
        if (zone.dataset.fileuploaderConfirm && !window.confirm(zone.dataset.fileuploaderConfirm)) {
            if (input) input.value = '';
            return;
        }
        if (!zone.id) zone.id = 'fileuploader-' + crypto.randomUUID();
        const batch = { zone, refresh: options.refresh, options: { ...options, refresh: false }, items: files.map(file => ({file, state: 'queued', percentage: 0})) };
        batches.set(zone.id, batch);
        renderBatch(batch);
        startBatch(batch);
    }

    // Editor adapters reserve their own insertion positions, but share the batch lifecycle.
    window.z_fileuploader = {
        clearDragState: reset,
        editorUpload({zone, files, options, reserve}) {
            if (pending.has(zone.id) || !files.length) return false;
            const error = validationError(zone, files);
            if (error) { report(zone, error, 'error'); return false; }
            const insertion = reserve();
            if (!insertion) { report(zone, message('editorNoPosition'), 'error'); return false; }
            batches.get(zone.id)?.dispose?.();
            const batch = {zone, options, editor: true, ...insertion,
                items: files.map((file, index) => ({file, index, state: 'queued', percentage: 0}))};
            batches.set(zone.id, batch);
            renderBatch(batch);
            startBatch(batch);
            return true;
        }
    };

    function renderBatch(batch) {
        const status = batch.zone.querySelector('.fileuploader-status');
        if (!status) return;
        status.classList.toggle('has-results', true);
        batch.zone.querySelector('.fileuploader-results')?.remove();
        const results = document.createElement('div');
        results.className = 'fileuploader-results';
        batch.items.forEach(item => {
            const row = document.createElement('div');
            row.className = 'fileuploader-result';
            const name = document.createElement('strong');
            name.textContent = item.file.name;
            const state = document.createElement('span');
            state.className = 'fileuploader-result-state';
            const progress = document.createElement('progress');
            progress.max = 100;
            progress.setAttribute('aria-label', item.file.name);
            row.append(state, name, progress);
            results.append(row);
            Object.assign(item, {row, label: state, progress});
        });
        const retry = document.createElement('button');
        retry.type = 'button';
        retry.className = 'btn btn-default';
        retry.textContent = message('retry');
        retry.hidden = true;
        retry.addEventListener('click', () => {
            if (pending.has(batch.zone.id)) return;
            if (batch.form) {
                batch.form.requestSubmit();
                return;
            }
            if (batch.zone.dataset.fileuploaderConfirm && !window.confirm(batch.zone.dataset.fileuploaderConfirm)) return;
            batch.items.filter(item => item.state === 'failed').forEach(item => {
                item.state = 'queued';
                item.error = '';
                item.percentage = 0;
            });
            startBatch(batch);
        });
        results.append(retry);
        const cancel = document.createElement('button');
        cancel.type = 'button';
        cancel.className = 'btn btn-default';
        cancel.textContent = message('cancel');
        cancel.addEventListener('click', () => {
            batch.items.filter(item => item.state === 'queued').forEach(item => item.state = 'cancelled');
            if (batch.current.state === 'uploading') {
                batch.cancelling = true;
                cotonic.broker.publish('model/fileuploader/post/cancel', {target: batch.zone.id, upload_id: batch.attempt});
            }
            updateBatch(batch);
        });
        results.append(cancel);
        batch.cancel = cancel;
        if (batch.refresh) {
            const refresh = document.createElement('button');
            refresh.type = 'button';
            refresh.className = 'btn btn-primary';
            refresh.textContent = message('refresh');
            refresh.addEventListener('click', () => window.location.reload());
            results.append(refresh);
            batch.refreshButton = refresh;
        }
        status.after(results);
        batch.retry = retry;
    }

    function updateBatch(batch) {
        batch.items.forEach(item => {
            if (!item.label) return;
            item.label.textContent = message(item.state === 'uploading' ? 'transferring' : item.state) + (item.state === 'uploading' ? ' ' + item.percentage + '%' : '') + (item.error ? ': ' + item.error : '');
            item.row.dataset.state = item.state;
            item.progress.value = item.percentage;
            item.progress.hidden = !['queued', 'uploading', 'processing'].includes(item.state);
        });
        if (batch.cancel) batch.cancel.hidden = !pending.has(batch.zone.id) || batch.cancelling
            || !batch.items.some(item => ['queued', 'uploading'].includes(item.state));
        if (batch.retry) batch.retry.hidden = pending.has(batch.zone.id) || !batch.items.some(item => item.state === 'failed');
        if (batch.refreshButton) batch.refreshButton.hidden = pending.has(batch.zone.id);
    }

    function startBatch(batch) {
        const {zone} = batch;
        pending.set(zone.id, zone);
        zone.setAttribute('aria-busy', 'true');
        report(zone, message(batch.editor ? 'editorUploading' : 'uploading'), 'uploading');
        nextFile(batch);
    }

    async function nextFile(batch) {
        const {zone} = batch;
        const item = batch.items.find(item => item.state === 'queued');
        if (!item) {
            pending.delete(zone.id);
            zone.removeAttribute('aria-busy');
            const input = zone.querySelector('input[type="file"]');
            if (input) input.value = '';
            const failed = batch.items.some(item => ['failed', 'unattached', 'notInserted'].includes(item.state));
            const summary = message('summary')
                .replace('{saved}', batch.items.filter(item => ['saved', 'unattached', 'notInserted'].includes(item.state)).length)
                .replace('{total}', batch.items.length);
            const cancelled = batch.items.some(item => item.state === 'cancelled');
            report(zone, summary + ' ' + message(failed ? 'processingFailure' : cancelled ? 'cancelledSummary' : batch.editor ? 'editorFinished' : 'finished'), failed ? 'error' : cancelled ? 'info' : 'success');
            // Release successful File objects; retries retain only failed files.
            batch.items.filter(item => item.state !== 'failed').forEach(item => item.file = {name: item.file.name});
            if (!batch.items.some(item => item.state === 'failed')) batch.dispose?.();
            updateBatch(batch);
            return;
        }
        batch.cancelling = false;
        batch.current = item;
        batch.attempt = String(++attempt);
        item.state = 'uploading';
        updateBatch(batch);
        try {
            await cotonic.ready;
            if (batch.cancelling) {
                batch.current.state = 'cancelled';
                nextFile(batch);
                return;
            }
            cotonic.broker.publish('model/fileuploader/post/new', {
                files: [{name: '0000000000', file: item.file}],
                ready_topic: 'bridge/origin/zotonic-transport/mod_fileuploader',
                ready_msg: {cmd: 'drop', target: zone.id, upload_id: batch.attempt, data: batch.options},
                progress_topic: 'model/fileuploader/event/admin-progress',
                progress_msg: {target: zone.id, upload_id: batch.attempt},
                failure_topic: 'model/fileuploader/event/admin-failure',
                failure_msg: {target: zone.id, upload_id: batch.attempt}
            });
        } catch (_) {
            finish(zone.id, true, batch.attempt);
        }
    }

    function finish(target, failed, uploadId, saved = false, error, mediaId) {
        const batch = batches.get(target);
        if (!pending.has(target) || !batch || (uploadId && uploadId !== batch.attempt)) return;
        batch.current.error = uploadError(error);
        batch.current.state = failed ? (saved ? 'unattached' : 'failed') : 'saved';
        if (!failed && batch.editor) {
            try {
                if (!Number.isSafeInteger(mediaId) || !batch.insert(mediaId, batch.current.index)) batch.current.state = 'notInserted';
            } catch (_) { batch.current.state = 'notInserted'; }
        }
        batch.current.percentage = failed ? batch.current.percentage : 100;
        nextFile(batch);
    }

    function startDialog({target, upload_id: uploadId, files}) {
        const form = document.getElementById(target);
        if (!form?.hasAttribute('data-fileuploader-feedback')) return;
        const zone = form.closest('[data-fileuploader-input]');
        const items = files.map(({file}) => ({file, state: 'uploading', percentage: 0}));
        if (!items.length) return;
        const batch = {zone: form, form, inputZone: zone, items, current: items[0], attempt: uploadId};
        batches.set(target, batch);
        pending.set(target, form);
        form.setAttribute('aria-busy', 'true');
        zone?.setAttribute('aria-busy', 'true');
        // Form values have already been serialized by the existing transport.
        // Keep fieldsets enabled so the newly rendered Cancel button remains usable.
        batch.controls = Array.from(form.elements).filter(el => el.tagName !== 'FIELDSET' && !el.disabled);
        batch.controls.forEach(el => el.disabled = true);
        z_unmask(target);
        renderBatch(batch);
        report(form, message('uploading'), 'uploading');
        updateBatch(batch);
    }

    function finishDialog({target, upload_id: uploadId, failed, error, cancelled}) {
        const batch = batches.get(target);
        if (!batch?.form || !pending.has(target) || uploadId !== batch.attempt) return;
        pending.delete(target);
        batch.form.removeAttribute('aria-busy');
        batch.inputZone?.removeAttribute('aria-busy');
        batch.controls.forEach(el => el.disabled = false);
        z_unmask(target);
        batch.current.state = cancelled ? 'cancelled' : failed ? 'failed' : 'saved';
        if (!failed) batch.current.percentage = 100;
        report(batch.form, cancelled ? message('cancelledSummary') : failed ? (uploadError(error) || error || message('failure')) : message('finished'), cancelled ? 'info' : failed ? 'error' : 'success');
        updateBatch(batch);
        // Success keeps the normal close/redirect actions; retain failed inputs for retry.
        if (!failed) batches.delete(target);
    }

    cotonic.ready.then(() => {
        cotonic.broker.subscribe('model/fileuploader/event/cancelled', ({payload}) => {
            const batch = batches.get(payload.target);
            if (!batch || !pending.has(payload.target) || payload.upload_id !== batch.attempt) return;
            batch.cancelling = false;
            if (!payload.cancelled) {
                batch.current.state = 'processing';
                updateBatch(batch);
            } else if (batch.form) {
                finishDialog({...payload, cancelled: true});
            } else {
                batch.current.state = 'cancelled';
                nextFile(batch);
            }
        });
        cotonic.broker.subscribe('model/fileuploader/event/dialog-start', msg => startDialog(msg.payload));
        cotonic.broker.subscribe('model/fileuploader/event/dialog-failure', msg => finishDialog({...msg.payload, failed: true}));
        cotonic.broker.subscribe('model/fileuploader/event/dialog-complete', msg => finishDialog(msg.payload));
        cotonic.broker.subscribe('model/fileuploader/event/admin-progress', ({payload}) => {
            const batch = batches.get(payload.target);
            if (!batch || !pending.has(payload.target) || payload.upload_id !== batch.attempt) return;
            batch.current.percentage = Math.max(0, Math.min(100, Number(payload.percentage) || 0));
            batch.current.state = batch.current.percentage === 100 ? 'processing' : 'uploading';
            updateBatch(batch);
        });
        cotonic.broker.subscribe('model/fileuploader/event/busy', msg => {
            uploaderBusy = msg.payload;
        });
        cotonic.broker.subscribe('model/fileuploader/event/admin-failure', msg => finish(msg.payload.target, true, msg.payload.upload_id, false, msg.payload.error));
        cotonic.broker.subscribe('model/fileuploader/event/admin-complete', msg => finish(msg.payload.target, msg.payload.failed, msg.payload.upload_id, msg.payload.saved, undefined, msg.payload.media_id));
    });

    // Keep direct uploads protected through server-side processing as well.
    // The worker also covers uploads submitted by the shared upload dialog.
    window.addEventListener('beforeunload', event => {
        if (pending.size || uploaderBusy) {
            event.preventDefault();
            event.returnValue = '';
        }
    });

    document.addEventListener('submit', event => {
        if (pending.has(event.target.id)) {
            event.preventDefault();
            event.stopImmediatePropagation();
        } else if (event.target.hasAttribute?.('data-fileuploader-feedback')) {
            const zone = event.target.closest('[data-fileuploader-input]');
            const input = zone?.querySelector('input[type="file"]');
            if (input?.files.length) {
                const error = validationError(zone, Array.from(input.files));
                if (error) {
                    event.preventDefault();
                    event.stopImmediatePropagation();
                    report(zone, error, 'error');
                }
            }
        }
    }, true);

    document.addEventListener('change', event => {
        if (!event.target.matches('input[type="file"]')) return;
        const zone = zoneFor(event.target);
        if (zone && zone !== document.body) {
            // Independent uploads must not dirty or submit the surrounding edit form.
            if (!zone.hasAttribute('data-fileuploader-input')) event.stopPropagation();
            selectFiles(zone, Array.from(event.target.files), false);
        }
    }, true);
    document.addEventListener('dragenter', event => {
        if (containsFiles(event)) dragDepth++;
    });
    document.addEventListener('dragover', event => {
        if (!containsFiles(event) || event.defaultPrevented) return;
        event.preventDefault(); // Never navigate away when files miss a zone.
        const zone = zoneFor(event.target);
        highlighted?.classList.remove('is-dragover', 'dragover');
        highlighted = zone;
        zone?.classList.add('is-dragover');
        if (zone === document.body) zone.classList.add('dragover');
        event.dataTransfer.dropEffect = zone && zone.getAttribute('aria-busy') !== 'true' ? 'copy' : 'none';
    });
    document.addEventListener('dragleave', event => {
        if (containsFiles(event) && --dragDepth <= 0) reset();
    });
    document.addEventListener('drop', event => {
        if (!containsFiles(event) || event.defaultPrevented) return;
        event.preventDefault();
        const zone = zoneFor(event.target);
        reset();
        if (!zone) return;
        const items = Array.from(event.dataTransfer.items || []);
        if (items.some(item => item.webkitGetAsEntry?.()?.isDirectory)) {
            report(zone, message('folder'), 'error');
            return;
        }
        selectFiles(zone, Array.from(event.dataTransfer.files), true);
    });
    window.addEventListener('blur', reset);
    document.addEventListener('dragend', reset);
})();
