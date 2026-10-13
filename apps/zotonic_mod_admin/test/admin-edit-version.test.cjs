/* Run with NODE_PATH pointing to the Tiptap package's node_modules (jsdom). */
const { test } = require('node:test');
const assert = require('node:assert/strict');
const { readFileSync } = require('node:fs');
const { join } = require('node:path');
const { JSDOM } = require('jsdom');
const source = readFileSync(join(__dirname, '../priv/lib/js/z.admin-edit-version.js'), 'utf8');

function fixture(t, tag = 'form') {
    const dom = new JSDOM(`<${tag} id="edit"><input name="z_edit_version" value="4"><div data-edit-version-warning hidden><button type="button" data-edit-version-dismiss>Close</button></div></${tag}>`, { runScripts: 'outside-only' });
    t.after(() => dom.window.close());
    let listener, reads = 0;
    const window = dom.window;
    window.cotonic = { broker: {
        subscribe(topics, callback) { listener = callback; },
        unsubscribe() {},
        call() { reads++; return Promise.resolve({ payload: { status: 'ok', result: 4 } }); }
    } };
    window.eval(source);
    window.z_admin_edit_version.watch('edit', 12);
    const form = window.document.getElementById('edit');
    return { window, form, input: form.querySelector('input[name="z_edit_version"]'),
        warning: form.querySelector('[data-edit-version-warning]'),
        update: version => listener({ payload: { version } }),
        processingUpdate: (before, version, reason = 'media_processing') => listener({
            topic: 'bridge/origin/model/rsc/event/12/update',
            payload: { id: 12, previous_version: before, version, reason } }),
        mediaUpdate: (before, version, id = 12) => listener({ topic: 'model/admin/event/media-version',
            payload: { id, previous_version: before, version } }),
        reads: () => reads,
        submit: () => form.dispatchEvent(new window.Event('submit')) };
}

test('updates warn without advancing the form version; old events are ignored', t => {
    const f = fixture(t);
    f.update(3);
    assert.equal(f.warning.hidden, true);
    f.update(5);
    assert.equal(f.warning.hidden, false);
    assert.equal(f.input.value, '4');
});

test('overview containers warn and do not accept acknowledgements from edit forms', t => {
    const f = fixture(t, 'div');
    f.update(5);
    assert.equal(f.warning.hidden, false);
    f.window.z_admin_edit_version.saved('rscform', 12, 5);
    assert.equal(f.input.value, '4');
    assert.equal(f.warning.hidden, false);
});

test('dismissal hides only the observed version without changing the save baseline', t => {
    const f = fixture(t);
    f.update(5);
    f.form.querySelector('[data-edit-version-dismiss]').click();
    assert.equal(f.warning.hidden, true);
    assert.equal(f.input.value, '4');
    f.update(5);
    assert.equal(f.warning.hidden, true);
    f.update(6);
    assert.equal(f.warning.hidden, false);
});

test('own translation saves acknowledge only the overview version they were based on', t => {
    const f = fixture(t, 'div');
    f.form.setAttribute('data-edit-version-overview', '');
    f.update(5);
    f.window.z_admin_edit_version.savedOverview(12, '4', 5);
    assert.equal(f.input.value, '5');
    assert.equal(f.warning.hidden, true);
    f.update(5);
    assert.equal(f.warning.hidden, true);
    f.update(7);
    f.window.z_admin_edit_version.savedOverview(12, '5', 6);
    assert.equal(f.input.value, '6');
    assert.equal(f.warning.hidden, false);
    f.window.z_admin_edit_version.savedOverview(12, '7', 8);
    assert.equal(f.input.value, '6');
    assert.equal(f.warning.hidden, false);
});

test('translation save acknowledgements do not advance other edit forms', t => {
    const f = fixture(t);
    f.update(5);
    f.window.z_admin_edit_version.savedOverview(12, '4', 5);
    assert.equal(f.input.value, '4');
    assert.equal(f.warning.hidden, false);
});

test('own save acknowledgement advances the baseline, but not past newer external edits', t => {
    const f = fixture(t);
    f.submit();
    f.update(5);
    assert.equal(f.warning.hidden, true);
    f.window.z_admin_edit_version.saved('edit', 12, 5);
    assert.equal(f.input.value, '5');
    assert.equal(f.warning.hidden, true);
    f.submit();
    f.update(7);
    f.window.z_admin_edit_version.saved('edit', 12, 6);
    assert.equal(f.warning.hidden, false);
    assert.equal(f.input.value, '6');
});

test('failed saves keep the original version and reveal conflicts', t => {
    const f = fixture(t);
    f.submit();
    f.update(5);
    f.window.z_admin_edit_version.saved('edit', 12, null);
    assert.equal(f.input.value, '4');
    assert.equal(f.warning.hidden, false);
});

test('media and reconnect notifications without versions recheck the resource', t => {
    const f = fixture(t);
    const before = f.reads();
    f.update(undefined);
    assert.equal(f.reads(), before + 1);
});

test('originating media replacements advance the edit version, without masking newer updates', t => {
    const f = fixture(t);
    f.update(5);
    f.mediaUpdate(4, 5);
    assert.equal(f.input.value, '5');
    assert.equal(f.warning.hidden, true);
    f.update(7);
    f.mediaUpdate(5, 6);
    assert.equal(f.input.value, '6');
    assert.equal(f.warning.hidden, false);
    f.mediaUpdate(7, 8);
    assert.equal(f.input.value, '6');
    assert.equal(f.warning.hidden, false);
});

test('media acknowledgements ignore other resources, overview pages and translation dialogs', t => {
    const form = fixture(t);
    form.mediaUpdate(4, 5, 99);
    assert.equal(form.input.value, '4');
    const overview = fixture(t, 'div');
    overview.mediaUpdate(4, 5);
    assert.equal(overview.input.value, '4');
    form.form.setAttribute('data-translation-edit-form', '');
    form.mediaUpdate(4, 5);
    assert.equal(form.input.value, '4');
});

test('late processing advances consecutive versions on edit pages and overviews', t => {
    for (const tag of ['form', 'div']) {
        const f = fixture(t, tag);
        f.update(5); // A polling response can arrive before the reasoned notification.
        f.processingUpdate(4, 5);
        assert.equal(f.input.value, '5');
        assert.equal(f.warning.hidden, true);
        f.processingUpdate(5, 6);
        assert.equal(f.input.value, '6');
        assert.equal(f.warning.hidden, true);
        f.processingUpdate(4, 5); // Duplicate or late notification.
        assert.equal(f.input.value, '6');
    }
});

test('processing does not hide intervening or later edits, or skip versions', t => {
    const f = fixture(t);
    f.processingUpdate(5, 6);
    assert.equal(f.input.value, '4');
    assert.equal(f.warning.hidden, false);
    f.processingUpdate(4, 5);
    assert.equal(f.input.value, '5');
    assert.equal(f.warning.hidden, false);
    f.processingUpdate(5, 6, 'update'); // Processing changed resource fields too.
    assert.equal(f.input.value, '5');
    assert.equal(f.warning.hidden, false);
    f.processingUpdate(5, 7);
    assert.equal(f.input.value, '5');
});
