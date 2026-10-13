/* Run with NODE_PATH pointing to the Tiptap package's node_modules (jsdom). */
const { test } = require('node:test');
const assert = require('node:assert/strict');
const { readFileSync } = require('node:fs');
const { join } = require('node:path');
const { JSDOM } = require('jsdom');
const source = readFileSync(join(__dirname, '../priv/lib/js/z.translation-edit.js'), 'utf8');

function fixture(t, tag = 'textarea') {
    const dom = new JSDOM(`<form><${tag} id="text"></${tag}>
        <div data-translation-source data-id="12" data-property="body" data-block="intro"
            data-language="nl" data-field="text" data-confirm="Replace?"
            data-busy="Busy" data-error="Error" data-changed="Changed">
            <select data-translation-language><option value="en">English</option></select>
            <button type="button" data-translation-start>Translate</button>
            <p data-translation-status></p>
        </div><button type="submit">Save</button></form>`, { runScripts: 'outside-only' });
    t.after(() => dom.window.close());
    const { window } = dom;
    window.$ = value => ({ value, on() {} });
    window.confirm = () => true;
    let resolve, reject, request, calls = 0;
    window.cotonic = { broker: { call(topic, payload) {
        calls++; request = { topic, payload };
        return new Promise((res, rej) => { resolve = res; reject = rej; });
    } } };
    window.eval(source);
    const field = window.document.getElementById('text');
    field.value = 'Original';
    const button = window.document.querySelector('[data-translation-start]');
    return { window, field, button,
        status: window.document.querySelector('[data-translation-status]'),
        save: window.document.querySelector('[type="submit"]'),
        result: text => resolve({ payload: { status: 'ok', result: text } }),
        fail: () => reject(new Error('Unavailable')), calls: () => calls, request: () => request,
        tick: () => new Promise(resolve => setImmediate(resolve)) };
}

test('translation fills plain inputs and textareas without submitting or interpreting HTML', async t => {
    for (const tag of ['input', 'textarea']) {
        const f = fixture(t, tag);
        let changes = 0;
        f.field.addEventListener('change', () => changes++);
        f.button.click();
        assert.equal(f.save.disabled, true);
        assert.equal(f.request().payload.from, 'en');
        assert.equal(f.request().payload.block, 'intro');
        f.result('<strong>Translated</strong>');
        await f.tick();
        assert.equal(f.field.value, '<strong>Translated</strong>');
        assert.equal(changes, 1);
        assert.equal(f.save.disabled, false);
    }
});

test('translation uses the Tiptap content API and synchronizes the textarea', async t => {
    const f = fixture(t);
    let content = 'Original';
    f.window.z_editor_tiptap = {
        get: () => ({}), setContent: (id, text) => { assert.equal(id, 'text'); content = text; return true; },
        save: () => { f.field.value = content; }
    };
    f.window.z_editor = f.window.z_editor_tiptap;
    f.button.click();
    f.result('<p>Translated</p>');
    await f.tick();
    assert.equal(content, '<p>Translated</p>');
    assert.equal(f.field.value, content);
});

test('failures, intervening edits and closed dialogs leave the text untouched', async t => {
    const failed = fixture(t);
    failed.button.click(); failed.fail(); await failed.tick();
    assert.equal(failed.field.value, 'Original');
    assert.equal(failed.status.textContent, 'Error');
    assert.equal(failed.save.disabled, false);
    const edited = fixture(t);
    edited.button.click(); edited.field.value = 'Manual edit';
    edited.result('Late result'); await edited.tick();
    assert.equal(edited.field.value, 'Manual edit');
    assert.equal(edited.status.textContent, 'Changed');
    const closed = fixture(t);
    closed.button.click(); closed.field.closest('form').remove();
    closed.result('Late result'); await closed.tick();
    assert.equal(closed.field.value, 'Original');
});

test('cancelled replacement and readonly fields do not call translation services', t => {
    const f = fixture(t);
    f.window.confirm = () => false;
    f.button.click();
    assert.equal(f.calls(), 0);
    f.window.confirm = () => true;
    f.field.readOnly = true;
    f.button.click();
    assert.equal(f.calls(), 0);
});
