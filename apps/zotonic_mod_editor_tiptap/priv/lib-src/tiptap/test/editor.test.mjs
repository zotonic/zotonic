import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { JSDOM } from 'jsdom';
import { cleanPaste, importHTML, exportHTML, safeUrl } from '../content.js';

const bundle = await readFile(new URL('../../../lib/js/zotonic-editor-tiptap.js', import.meta.url), 'utf8');
const parsing = new JSDOM('');
globalThis.document = parsing.window.document;
globalThis.NodeFilter = parsing.window.NodeFilter;

function page(content = '<p>Hello world</p>', configuration = {}) {
    const dom = new JSDOM('<form id="form"><label for="body-en">Body</label><textarea id="body-en" name="body$en" lang="en" class="z_editor-init"></textarea><section hidden id="nl"><textarea id="body-nl" name="body$nl" lang="nl" class="z_editor-init"></textarea></section></form>', {
        url: 'https://example.test/admin/edit/1', pretendToBeVisual: true, runScripts: 'outside-only'
    });
    const { window } = dom;
    window.scrollTo = () => {};
    window.HTMLElement.prototype.getClientRects = function() { return this.hidden || this.closest('[hidden]') ? [] : [{ top: 0, bottom: 100 }]; };
    window.HTMLElement.prototype.getBoundingClientRect = () => ({ top: 0, bottom: 100, left: 0, right: 400, width: 400, height: 100 });
    window.Range.prototype.getClientRects = () => [];
    window.Range.prototype.getBoundingClientRect = () => ({ top: 0, bottom: 0, left: 0, right: 0 });
    window.document.querySelector('#body-en').value = content;
    window.document.querySelector('#body-nl').value = '<p>Nederlands</p>';
    const events = [];
    window.z_event = (name, args) => events.push({ name, args });
    window.z_notify = (name, args) => events.push({ name, args });
    window.z_dialog_close = () => events.push({ name: 'close' });
    window.z_editor_tiptap_config = configuration;
    window.eval(bundle);
    window.z_editor.init();
    return { window, events, api: window.z_editor, close: () => { window.z_editor.remove(); window.close(); } };
}

test('media comments survive import/export, including Unicode and comment delimiters', () => {
    const options = { caption: 'Caf\u00e9 - title', align: 'left', crop: 'crop', extra: { value: 2 } };
    const html = `<p>Before</p><!-- z-media 123 ${JSON.stringify(options)} --><p>After</p>`;
    const imported = importHTML(html);
    assert.match(imported, /data-zmedia-id="123"/);
    const exported = exportHTML({ getHTML: () => imported });
    const marker = exported.match(/<!-- z-media 123 (.*?) -->/);
    assert.deepEqual(JSON.parse(marker[1]), options);
    assert.equal(importHTML(exported), imported);
    const hostile = importHTML('<!-- z-media 1 {"caption":"a\\u002d\\u002d>b"} -->');
    assert.match(exportHTML({ getHTML: () => hostile }), /\\u002d\\u002d/);
});

test('unsafe URL protocols are rejected', () => {
    for (const url of ['javascript:alert(1)', 'java\nscript:alert(1)', 'data:text/html,evil', 'vbscript:evil']) assert.equal(safeUrl(url), '');
    for (const url of ['/page/1', '#anchor', 'https://example.test', 'mailto:test@example.test', 'relative-page']) assert.equal(safeUrl(url), url);
});

test('Office nested list paragraphs become semantic lists', () => {
    const result = cleanPaste('<p style="mso-list:l0 level1 lfo1"><span style="mso-list:Ignore">1.</span>First</p><p style="mso-list:l0 level2 lfo1"><span style="mso-list:Ignore">a.</span>Child</p><p style="mso-list:l0 level1 lfo1"><span style="mso-list:Ignore">2.</span>Second</p>');
    assert.equal(result, '<ol><li>First<ol><li>Child</li></ol></li><li>Second</li></ol>');
});

test('paste cleanup keeps emphasis and removes Office fonts/classes', () => {
    const html = cleanPaste('<p class="MsoNormal" style="font-family:Calibri;font-weight:bold;text-align:center">Text</p>');
    assert.match(html, /font-weight:bold/);
    assert.match(html, /text-align:center/);
    assert.doesNotMatch(html, /Mso|Calibri/);
});

test('initialization is idempotent, preserves untouched HTML, and needs no TinyMCE', () => {
    const original = '<p class="intro">Original &amp; unchanged</p>\n';
    const p = page(original);
    try {
        assert.equal(p.window.tinymce, undefined);
        assert.ok(p.api.get('body-en'));
        assert.equal(p.api.get('body-nl'), undefined);
        p.api.init(); p.api.add(p.window.document.querySelector('form')); p.api.save();
        assert.equal(p.window.document.querySelectorAll('.z-tiptap').length, 1);
        assert.equal(p.window.document.querySelector('#body-en').value, original);
        assert.equal(p.window.document.querySelector('[role="textbox"]').getAttribute('aria-label'), 'Body');
    } finally { p.close(); }
});

test('language tab initialization retains the first editor and its undo history', () => {
    const p = page();
    try {
        const first = p.api.get('body-en');
        first.commands.insertContent('Added ');
        p.window.document.querySelector('#nl').hidden = false;
        p.api.init();
        assert.ok(p.api.get('body-nl'));
        assert.equal(p.api.get('body-en'), first);
        first.commands.undo();
        assert.equal(first.getText(), 'Hello world');
    } finally { p.close(); }
});

test('scoped saving, removal and remount keep edited content', () => {
    const p = page();
    try {
        const textarea = p.window.document.querySelector('#body-en');
        p.api.get('body-en').commands.insertContent('Edited ');
        p.api.save(p.window.document.querySelector('#nl'));
        assert.equal(textarea.value, '<p>Hello world</p>');
        p.api.remove(textarea);
        assert.match(textarea.value, /Edited/);
        assert.equal(textarea.hidden, false);
        assert.equal(p.api.get('body-en'), undefined);
        p.api.add(textarea);
        assert.match(p.api.get('body-en').getText(), /Edited/);
    } finally { p.close(); }
});

test('link dialogs return to the originating editor after language switching', () => {
    const p = page();
    try {
        const editor = p.api.get('body-en');
        editor.commands.setTextSelection({ from: 1, to: 6 });
        p.window.document.querySelector('[data-command="zlink"]').click();
        assert.equal(p.events[0].name, 'zlink');
        assert.equal(p.events[0].args.language, 'en');
        p.window.document.querySelector('#nl').hidden = false;
        p.api.init();
        p.api.linkDone({ url_language: '/page/2', title_language: 'Page' });
        p.api.save();
        assert.match(p.window.document.querySelector('#body-en').value, /href="\/page\/2"/);
        assert.equal(p.window.document.querySelector('#body-nl').value, '<p>Nederlands</p>');
    } finally { p.close(); }
});

test('media insertion and properties preserve markers and unknown media options', () => {
    const p = page('<!-- z-media 12 {"caption":"Caption","custom":"keep"} -->');
    try {
        const editor = p.api.get('body-en');
        editor.commands.setNodeSelection(0);
        p.window.document.querySelector('[data-command="zmedia"]').click();
        const request = p.events.find(event => event.name === 'media_props');
        assert.equal(request.args.id, '12');
        const form = p.window.document.createElement('form');
        form.dataset.tiptapMediaRequest = request.args.request_id;
        form.innerHTML = '<textarea name="caption">New caption</textarea><select name="align"><option value="right">Right</option></select>';
        p.window.document.body.append(form);
        form.dispatchEvent(new p.window.Event('submit', { bubbles: true, cancelable: true }));
        p.api.save();
        const html = p.window.document.querySelector('#body-en').value;
        assert.match(html, /<!-- z-media 12/);
        assert.match(html, /New caption/);
        assert.match(html, /"custom":"keep"/);
        editor.commands.undo();
        assert.equal(editor.getAttributes('zotonicMedia').options.caption, 'Caption');
    } finally { p.close(); }
});

test('removed editors ignore stale dialog callbacks and release DOM', () => {
    const p = page();
    try {
        p.window.document.querySelector('[data-command="zmedia"]').click();
        p.api.remove();
        p.api.mediaDone({ object_id: 123 });
        assert.equal(p.window.document.querySelectorAll('.z-tiptap').length, 0);
    } finally { p.close(); }
});

test('named profiles change toolbar without dropping supported content', () => {
    const p = page('<h2>Heading</h2><table><tbody><tr><td><p>Cell</p></td></tr></tbody></table>', { body: { toolbar: ['bold'] } });
    try {
        assert.equal(p.window.document.querySelectorAll('.z-tiptap-toolbar button').length, 1);
        assert.match(p.api.get('body-en').getHTML(), /<table/);
    } finally { p.close(); }
});

test('full window keeps the same instance and exits with Escape', () => {
    const p = page();
    try {
        const editor = p.api.get('body-en');
        p.window.document.querySelector('[data-command="fullscreen"]').click();
        assert.ok(p.window.document.querySelector('.z-tiptap.is-fullscreen'));
        editor.view.dom.dispatchEvent(new p.window.KeyboardEvent('keydown', { key: 'Escape', bubbles: true }));
        assert.equal(p.window.document.querySelector('.z-tiptap.is-fullscreen'), null);
        assert.equal(p.api.get('body-en'), editor);
    } finally { p.close(); }
});

test('dynamic removal destroys the corresponding editor', async () => {
    const p = page();
    try {
        const editor = p.api.get('body-en');
        p.window.document.querySelector('form').remove();
        await new Promise(resolve => setTimeout(resolve, 0));
        assert.equal(editor.isDestroyed, true);
    } finally { p.close(); }
});

test('required fields block submission, then save successfully after editing', () => {
    const p = page('');
    try {
        const textarea = p.window.document.querySelector('#body-en');
        p.api.remove(textarea);
        textarea.required = true;
        p.api.add(textarea);
        const form = textarea.form;
        assert.equal(form.dispatchEvent(new p.window.Event('submit', { bubbles: true, cancelable: true })), false);
        assert.ok(form.querySelector('[role="alert"]'));
        p.api.get('body-en').commands.insertContent('Required text');
        assert.equal(form.querySelector('[role="alert"]'), null);
        assert.equal(form.dispatchEvent(new p.window.Event('submit', { bubbles: true, cancelable: true })), true);
        assert.match(textarea.value, /Required text/);
        p.api.remove(textarea);
        assert.equal(textarea.required, true);
    } finally { p.close(); }
});

test('readonly changes do not dirty original HTML or allow pending dialog inserts', async () => {
    const original = '<p>Hello world</p>\n';
    const p = page(original);
    try {
        p.window.document.querySelector('[data-command="zmedia"]').click();
        p.window.document.querySelector('#body-en').readOnly = true;
        await new Promise(resolve => setTimeout(resolve, 0));
        assert.equal(p.api.get('body-en').isEditable, false);
        assert.equal(p.window.document.querySelector('[data-command="bold"]').disabled, true);
        p.api.mediaDone({ object_id: 123 });
        p.api.save();
        assert.equal(p.window.document.querySelector('#body-en').value, original);
    } finally { p.close(); }
});

test('form reset restores original text', async () => {
    const p = page();
    try {
        const textarea = p.window.document.querySelector('#body-en');
        textarea.defaultValue = '<p>Reset value</p>';
        p.api.get('body-en').commands.insertContent('Changed ');
        textarea.form.reset();
        await new Promise(resolve => setTimeout(resolve, 5));
        assert.equal(p.api.get('body-en').getText(), 'Reset value');
    } finally { p.close(); }
});

test('table controls insert a table, add a row, and remove the table', () => {
    const p = page();
    try {
        const menu = [...p.window.document.querySelectorAll('select')].find(el => el.getAttribute('aria-label') === 'table');
        const choose = value => { menu.value = value; menu.dispatchEvent(new p.window.Event('change')); };
        choose('insertTable');
        assert.equal(p.window.document.querySelectorAll('.z-tiptap-body tr').length, 3);
        choose('addRowAfter');
        assert.equal(p.window.document.querySelectorAll('.z-tiptap-body tr').length, 4);
        choose('deleteTable');
        assert.equal(p.window.document.querySelectorAll('.z-tiptap-body table').length, 0);
    } finally { p.close(); }
});
