/* Run with node --test apps/zotonic_mod_fileuploader/test/admin_dropzones_test.js */
const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync('apps/zotonic_mod_fileuploader/priv/lib/js/z.fileuploader-admin.js', 'utf8');

// Minimal event fixture: no browser packages are needed for routing/transport regressions.
async function fixture(options = {}) {
    const listeners = {}, subscriptions = {}, publications = [], masks = [], unmasks = [];
    const attrs = new Map(Object.entries(options.attributes || { 'data-fileuploader': '{}' }));
    const nodes = [];
    const createElement = tag => {
        const node = {tag, children: [], dataset: {}, listeners: {}, textContent: '',
            append(...children) { this.children.push(...children); },
            setAttribute() {}, remove() {}, addEventListener(name, fn) { this.listeners[name] = fn; }};
        nodes.push(node);
        return node;
    };
    const status = { textContent: '', classList: { toggle() {} }, after() {} };
    const classes = new Set();
    const zone = {
        id: 'zone', dataset: options.confirm ? {fileuploaderConfirm: 'Replace?'} : {},
        classList: { add: c => classes.add(c), remove: c => classes.delete(c) },
        getAttribute: key => attrs.get(key), hasAttribute: key => attrs.has(key),
        setAttribute: (key, value) => attrs.set(key, value), removeAttribute: key => attrs.delete(key),
        querySelector: selector => selector === '.fileuploader-status' ? status : selector === '.fileuploader-results' ? null : input,
        closest: () => zone
    };
    const input = {
        files: [], value: 'selected', accept: options.accept || '', multiple: false,
        matches: () => true, closest: () => zone,
        dispatchEvent: event => { event.target = input; listeners[event.type](event); }
    };
    const formAttrs = new Map();
    const fieldset = {tagName: 'FIELDSET', disabled: false};
    const title = {value: 'Keep this title', disabled: false};
    const published = {checked: false, disabled: false};
    const form = {
        id: 'dialog-form', elements: [fieldset, input, title, published], dataset: {}, submissions: 0,
        querySelector: zone.querySelector, closest: () => zone,
        hasAttribute: key => key === 'data-fileuploader-feedback' || formAttrs.has(key),
        setAttribute: (key, value) => formAttrs.set(key, value), removeAttribute: key => formAttrs.delete(key),
        requestSubmit() { this.submissions++; }
    };
    if (options.dialogForm) input.form = form;
    const modal = {getClientRects: () => [1], contains: el => options.insideModal && el === zone};
    const messages = Object.fromEntries(['cancel','cancelled','cancelledSummary','folder','oversized','single','empty','accept','failure','processingFailure','uploading','selected','finished','queued','processing','saved','failed','unattached','retry','transferring'].map(k => [k,k]));
    messages.maxSize = options.maxSize;
    const context = {
        document: {
            createElement,
            getElementById: id => id === form.id ? form : ({dataset: messages}),
            querySelectorAll: () => options.modal ? [modal] : [],
            addEventListener: (name, fn) => { listeners[name] = fn; }
        },
        window: {addEventListener: (name, fn) => { listeners[name] = fn; }, alert() {}, confirm: () => options.confirmed !== false},
        cotonic: {ready: Promise.resolve(), broker: {
            subscribe: (topic, fn) => { subscriptions[topic] = fn; },
            publish: (topic, payload) => { publications.push({topic, payload}); }
        }},
        z_mask: id => masks.push(id), z_unmask: id => unmasks.push(id),
        Event: class { constructor(type) { this.type = type; } },
        DataTransfer: class {
            constructor() { this.files = []; this.items = {add: file => this.files.push(file)}; }
        }
    };
    vm.runInNewContext(source, context);
    await new Promise(resolve => setImmediate(resolve));
    async function drop(files, target = zone, types = ['Files'], items = []) {
        const event = {target, dataTransfer: {types, files, items}, preventDefault() { this.prevented = true; }};
        listeners.drop(event);
        await new Promise(resolve => setImmediate(resolve));
        return event;
    }
    return {zone, input, status, publications, masks, unmasks, subscriptions, drop, listeners, classes, nodes, form, title, published, fieldset, uploader: context.window.z_fileuploader};
}
const file = (name = 'image.png', type = 'image/png', size = 10) => ({name, type, size});

test('contextual multi-file upload uses the existing worker and retains selection order', async () => {
    const f = await fixture();
    await f.drop([file('first'), file('second', '')]);
    const {topic, payload} = f.publications[0];
    assert.equal(topic, 'model/fileuploader/post/new');
    assert.equal(payload.ready_msg.target, 'zone');
    assert.equal(payload.files.length, 1);
    assert.equal(payload.files[0].file.name, 'first');
    f.subscriptions['model/fileuploader/event/admin-complete']({payload: {target: 'zone', failed: false, upload_id: payload.ready_msg.upload_id}});
    await new Promise(resolve => setImmediate(resolve));
    assert.equal(f.publications[1].payload.files[0].file.name, 'second');
    assert.deepEqual(f.masks, []);
});

test('replacement rejects several files before any upload', async () => {
    const f = await fixture({attributes: {'data-fileuploader': '{"is_medium":true}'}});
    await f.drop([file(), file()]);
    assert.equal(f.status.textContent, 'single');
    assert.equal(f.publications.length, 0);
});

test('replacement cancellation does not upload', async () => {
    const f = await fixture({confirm: true, confirmed: false});
    await f.drop([file()]);
    assert.equal(f.publications.length, 0);
});

test('dialog drop selects the input without uploading and preserves the submit workflow', async () => {
    const f = await fixture({modal: true, insideModal: true, attributes: {'data-fileuploader-input': ''}});
    await f.drop([file()]);
    assert.equal(f.input.files.length, 1);
    assert.equal(f.status.textContent, 'selected');
    assert.equal(f.publications.length, 0);
});

test('open dialog blocks underlying zones, including the legacy body target', async () => {
    const f = await fixture({modal: true});
    const event = await f.drop([file()]);
    assert.equal(event.prevented, true);
    assert.equal(f.publications.length, 0);
});

test('invalid dialog drops retain the previous selection and never fall through', async () => {
    const f = await fixture({attributes: {'data-fileuploader-input': ''}, accept: '.png,image/*'});
    f.input.files = [file('previous.png')];
    await f.drop([file('new.pdf', 'application/pdf')]);
    assert.equal(f.status.textContent, 'accept');
    assert.equal(f.input.files[0].name, 'previous.png');
    assert.equal(f.publications.length, 0);
});

test('folders and zero-byte files report errors instead of being silently skipped', async () => {
    const f = await fixture();
    await f.drop([file()], f.zone, ['Files'], [{webkitGetAsEntry: () => ({isDirectory: true})}]);
    assert.equal(f.status.textContent, 'folder');
    await f.drop([file('empty.txt', '', 0)]);
    assert.equal(f.publications.length, 0);
});

test('unhandled file drops prevent navigation but text and internal drags are untouched', async () => {
    const f = await fixture();
    const outside = {closest: () => null};
    assert.equal((await f.drop([file()], outside)).prevented, true);
    assert.equal((await f.drop([], outside, ['text/plain'])).prevented, undefined);
    assert.equal(f.publications.length, 0);
});

test('busy zones reject duplicate drops and failure restores the picker', async () => {
    const f = await fixture();
    await f.drop([file()]);
    await f.drop([file()]);
    assert.equal(f.publications.length, 1);
    f.subscriptions['model/fileuploader/event/admin-failure']({payload: {target: 'zone'}});
    assert.deepEqual(f.unmasks, []);
    assert.equal(f.zone.hasAttribute('aria-busy'), false);
    assert.equal(f.input.value, '');
    assert.equal(f.status.textContent.trim(), 'processingFailure');
    await f.drop([file()]);
    assert.equal(f.publications.length, 2);
});

test('server completion clears local progress after media processing', async () => {
    const f = await fixture();
    await f.drop([file()]);
    f.subscriptions['model/fileuploader/event/admin-complete']({payload: {target: 'zone', failed: false}});
    assert.equal(f.status.textContent.trim(), 'finished');
    assert.equal(f.zone.hasAttribute('aria-busy'), false);
});


test('independent picker changes do not reach the enclosing edit form', async () => {
    const f = await fixture();
    f.input.files = [file()];
    const event = {target: f.input, stopPropagation() { this.stopped = true; }};
    f.listeners.change(event);
    await new Promise(resolve => setImmediate(resolve));
    assert.equal(event.stopped, true);
    assert.equal(f.publications.length, 1);
});

test('navigation is guarded until direct uploads finish or fail', async () => {
    const f = await fixture();
    const leaving = () => {
        const event = {preventDefault() { this.prevented = true; }};
        f.listeners.beforeunload(event);
        return event;
    };
    assert.equal(leaving().prevented, undefined);
    await f.drop([file()]);
    assert.equal(leaving().prevented, true);
    f.subscriptions['model/fileuploader/event/admin-complete']({payload: {target: 'zone', failed: false}});
    assert.equal(leaving().prevented, undefined);
    await f.drop([file()]);
    f.subscriptions['model/fileuploader/event/admin-failure']({payload: {target: 'zone'}});
    assert.equal(leaving().prevented, undefined);
});

test('dialog selection does not guard navigation, but worker uploads do', async () => {
    const f = await fixture({attributes: {'data-fileuploader-input': ''}});
    await f.drop([file()]);
    const event = {preventDefault() { this.prevented = true; }};
    f.listeners.beforeunload(event);
    assert.equal(event.prevented, undefined);
    f.subscriptions['model/fileuploader/event/busy']({payload: true});
    f.listeners.beforeunload(event);
    assert.equal(event.prevented, true);
    f.subscriptions['model/fileuploader/event/busy']({payload: false});
    const finished = {preventDefault() { this.prevented = true; }};
    f.listeners.beforeunload(finished);
    assert.equal(finished.prevented, undefined);
});

test('mixed results retry only failed files and ignore stale attempt events', async () => {
    const f = await fixture();
    await f.drop([file('saved.png'), file('retry.png')]);
    const complete = payload => f.subscriptions['model/fileuploader/event/admin-complete']({payload});
    const first = f.publications[0].payload.ready_msg.upload_id;
    complete({target: 'zone', upload_id: first, failed: false});
    await new Promise(resolve => setImmediate(resolve));
    const second = f.publications[1].payload.ready_msg.upload_id;
    f.subscriptions['model/fileuploader/event/admin-progress']({payload: {target: 'zone', upload_id: second, percentage: 42}});
    assert.equal(f.nodes.filter(n => n.tag === 'progress')[1].value, 42);
    complete({target: 'zone', upload_id: second, failed: true});
    const retry = f.nodes.find(n => n.tag === 'button');
    assert.equal(retry.hidden, false);
    retry.listeners.click();
    await new Promise(resolve => setImmediate(resolve));
    assert.equal(f.publications.length, 3);
    assert.equal(f.publications[2].payload.files[0].file.name, 'retry.png');
    complete({target: 'zone', upload_id: second, failed: true});
    assert.equal(f.zone.hasAttribute('aria-busy'), true);
    complete({target: 'zone', upload_id: f.publications[2].payload.ready_msg.upload_id, failed: false});
    assert.equal(f.zone.hasAttribute('aria-busy'), false);
    assert.equal(retry.hidden, true);
});

test('a saved file with a failed connection is not reuploaded', async () => {
    const f = await fixture();
    await f.drop([file()]);
    f.subscriptions['model/fileuploader/event/admin-complete']({payload: {target: 'zone', failed: true, saved: true}});
    assert.equal(f.nodes.find(n => n.tag === 'button').hidden, true);
    assert.equal(f.nodes.find(n => n.className === 'fileuploader-result').dataset.state, 'unattached');
});

test('dialog progress waits for server result and retry preserves form values', async () => {
    const f = await fixture({dialogForm: true, attributes: {'data-fileuploader-input': ''}});
    f.input.files = [file('dialog.png')];
    const start = upload_id => f.subscriptions['model/fileuploader/event/dialog-start']({payload: {
        target: f.form.id, upload_id, files: [{file: f.input.files[0]}]
    }});
    start('first');
    assert.equal(f.input.disabled, true);
    assert.equal(f.zone.hasAttribute('aria-busy'), true);
    f.subscriptions['model/fileuploader/event/admin-progress']({payload: {
        target: f.form.id, upload_id: 'first', percentage: 100
    }});
    assert.equal(f.nodes.find(n => n.className === 'fileuploader-result').dataset.state, 'processing');
    const leaving = {preventDefault() { this.prevented = true; }};
    f.listeners.beforeunload(leaving);
    assert.equal(leaving.prevented, true);
    f.subscriptions['model/fileuploader/event/dialog-complete']({payload: {
        target: f.form.id, upload_id: 'first', failed: true, error: 'Rejected by server'
    }});
    assert.equal(f.input.disabled, false);
    assert.equal(f.input.files[0].name, 'dialog.png');
    assert.equal(f.title.value, 'Keep this title');
    assert.equal(f.published.checked, false);
    const retry = f.nodes.find(n => n.tag === 'button');
    assert.equal(retry.hidden, false);
    retry.listeners.click();
    assert.equal(f.form.submissions, 1);
    assert.equal(f.publications.length, 0); // Retry uses the signed form, not direct upload.
    start('retry');
    f.subscriptions['model/fileuploader/event/dialog-failure']({payload: {target: f.form.id, upload_id: 'first'}});
    assert.equal(f.form.hasAttribute('aria-busy'), true);
    f.subscriptions['model/fileuploader/event/dialog-complete']({payload: {
        target: f.form.id, upload_id: 'retry', failed: false
    }});
    assert.equal(f.form.hasAttribute('aria-busy'), false);
    assert.equal(f.status.textContent, 'finished');
    const finished = {preventDefault() { this.prevented = true; }};
    f.listeners.beforeunload(finished);
    assert.equal(finished.prevented, undefined);
});

test('dialog transport failure restores controls and offers retry', async () => {
    const f = await fixture({dialogForm: true, attributes: {'data-fileuploader-input': ''}});
    f.subscriptions['model/fileuploader/event/dialog-start']({payload: {
        target: f.form.id, upload_id: 'attempt', files: [{file: file()}]
    }});
    f.subscriptions['model/fileuploader/event/dialog-failure']({payload: {target: f.form.id, upload_id: 'attempt'}});
    assert.equal(f.status.textContent, 'failure');
    assert.equal(f.input.disabled, false);
    assert.equal(f.nodes.find(n => n.tag === 'button').hidden, false);
});


test('cancel stops queued files and waits for worker acknowledgement before clearing the guard', async () => {
    const f = await fixture();
    await f.drop([file('active'), file('queued')]);
    const attempt = f.publications[0].payload.progress_msg;
    f.nodes.find(n => n.tag === 'button' && n.textContent === 'cancel').listeners.click();
    assert.equal(f.publications[1].topic, 'model/fileuploader/post/cancel');
    let guarded = false;
    f.listeners.beforeunload({preventDefault() { guarded = true; }});
    assert(guarded);
    f.subscriptions['model/fileuploader/event/cancelled']({payload: {...attempt, cancelled: true}});
    assert.equal(f.nodes.filter(n => n.dataset.state === 'cancelled').length, 2);
    guarded = false;
    f.listeners.beforeunload({preventDefault() { guarded = true; }});
    assert.equal(guarded, false);
    assert.equal(f.publications.filter(p => p.topic.endsWith('/new')).length, 1);
});

test('cancellation after server handoff keeps processing guarded and cancels only queued files', async () => {
    const f = await fixture();
    await f.drop([file('saving'), file('queued')]);
    const attempt = f.publications[0].payload.progress_msg;
    f.nodes.find(n => n.tag === 'button' && n.textContent === 'cancel').listeners.click();
    f.subscriptions['model/fileuploader/event/cancelled']({payload: {...attempt, cancelled: false}});
    let guarded = false;
    f.listeners.beforeunload({preventDefault() { guarded = true; }});
    assert(guarded);
    f.subscriptions['model/fileuploader/event/admin-complete']({payload: {...attempt, failed: false}});
    assert.equal(f.nodes.filter(n => n.dataset.state === 'saved').length, 1);
    assert.equal(f.nodes.filter(n => n.dataset.state === 'cancelled').length, 1);
});

test('oversized selections are rejected before starting any upload', async () => {
    const f = await fixture({maxSize: 100});
    await f.drop([file('too-large', '', 101)]);
    assert.equal(f.status.textContent, 'oversized');
    assert.equal(f.publications.length, 0);
});


test('dialog cancellation restores input and metadata without offering a failed-file retry', async () => {
    const f = await fixture({dialogForm: true, attributes: {'data-fileuploader-input': ''}});
    f.input.files = [file()];
    const feedback = {target: f.form.id, upload_id: 'cancel-dialog'};
    f.subscriptions['model/fileuploader/event/dialog-start']({payload: {...feedback, files: [{file: file()}]}});
    assert.equal(f.fieldset.disabled, false);
    assert.equal(f.input.disabled, true);
    f.nodes.find(n => n.tag === 'button' && n.textContent === 'cancel').listeners.click();
    f.subscriptions['model/fileuploader/event/cancelled']({payload: {...feedback, cancelled: true}});
    assert.equal(f.input.disabled, false);
    assert.equal(f.input.files.length, 1);
    assert.equal(f.title.value, 'Keep this title');
    assert.equal(f.published.checked, false);
    assert.equal(f.form.hasAttribute('aria-busy'), false);
    assert.equal(f.nodes.find(n => n.tag === 'button' && n.textContent === 'retry').hidden, true);
    let guarded = false;
    f.listeners.beforeunload({preventDefault() { guarded = true; }});
    assert.equal(guarded, false);
});


test('editor batches insert server-confirmed ids once and retry only failed files', async () => {
    const f = await fixture();
    const inserted = [];
    let disposed = 0;
    f.uploader.editorUpload({zone: f.zone, files: [file('a'), file('b')], options: {subject_id: 12, is_dependent: true},
        reserve: () => ({insert: (id, index) => { inserted.push([id, index]); return true; }, dispose: () => disposed++})});
    await new Promise(resolve => setImmediate(resolve));
    const first = f.publications[0].payload.progress_msg;
    assert.equal(f.publications[0].payload.ready_msg.data.is_dependent, true);
    f.subscriptions['model/fileuploader/event/admin-failure']({payload: first});
    await new Promise(resolve => setImmediate(resolve));
    const second = f.publications[1].payload.progress_msg;
    f.subscriptions['model/fileuploader/event/admin-complete']({payload: {...second, failed: false, media_id: 102}});
    assert.deepEqual(inserted, [[102, 1]]);
    assert.equal(disposed, 0);
    f.nodes.find(n => n.tag === 'button' && n.textContent === 'retry').listeners.click();
    await new Promise(resolve => setImmediate(resolve));
    const retry = f.publications[2].payload.progress_msg;
    f.subscriptions['model/fileuploader/event/admin-complete']({payload: {...first, failed: false, media_id: 999}});
    f.subscriptions['model/fileuploader/event/admin-complete']({payload: {...retry, failed: false, media_id: 101}});
    assert.deepEqual(inserted, [[102, 1], [101, 0]]);
    assert.equal(disposed, 1);
});

test('a saved editor upload is never retried when its insertion position has gone', async () => {
    const f = await fixture();
    f.uploader.editorUpload({zone: f.zone, files: [file()], options: {}, reserve: () => ({insert: () => false})});
    await new Promise(resolve => setImmediate(resolve));
    f.subscriptions['model/fileuploader/event/admin-complete']({payload: {
        ...f.publications[0].payload.progress_msg, failed: false, media_id: 103
    }});
    assert.equal(f.nodes.find(n => n.dataset.state === 'notInserted').dataset.state, 'notInserted');
    assert.equal(f.nodes.find(n => n.tag === 'button' && n.textContent === 'retry').hidden, true);
});


test('an invalid new drop position preserves the previous failed upload for retry', async () => {
    const f = await fixture();
    let disposed = false;
    let inserted = false;
    f.uploader.editorUpload({zone: f.zone, files: [file()], options: {}, reserve: () => ({
        insert: () => { inserted = !disposed; return inserted; }, dispose: () => { disposed = true; }
    })});
    await new Promise(resolve => setImmediate(resolve));
    f.subscriptions['model/fileuploader/event/admin-failure']({payload: f.publications[0].payload.progress_msg});
    assert.equal(f.uploader.editorUpload({zone: f.zone, files: [file()], options: {}, reserve: () => null}), false);
    assert.equal(disposed, false);
    f.nodes.find(n => n.tag === 'button' && n.textContent === 'retry').listeners.click();
    await new Promise(resolve => setImmediate(resolve));
    f.subscriptions['model/fileuploader/event/admin-complete']({payload: {
        ...f.publications[1].payload.progress_msg, failed: false, media_id: 104
    }});
    assert.equal(inserted, true);
});
