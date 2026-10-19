import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { JSDOM } from 'jsdom';
import { cleanPaste, importHTML, exportHTML, safeUrl } from '../content.js';

const bundle = await readFile(new URL('../../../lib/js/zotonic-editor-tiptap.js', import.meta.url), 'utf8');
const profilesTemplate = await readFile(new URL('../../../templates/_editor_tiptap_profiles.tpl', import.meta.url), 'utf8');
const profilesScript = profilesTemplate.replace(/{#[\s\S]*?#}/g, '').replace(/{%\s*(?:block \w+|endblock)\s*%}/g, '');
const parsing = new JSDOM('');
globalThis.document = parsing.window.document;
globalThis.NodeFilter = parsing.window.NodeFilter;

function page(content = '<p>Hello world</p>', configuration = {}, profile = 'advanced') {
    const dom = new JSDOM('<form id="form"><label for="body-en">Body</label><textarea id="body-en" name="body$en" lang="en" class="z_editor-init"></textarea><section hidden id="nl"><textarea id="body-nl" name="body$nl" lang="nl" class="z_editor-init"></textarea></section></form>', {
        url: 'https://example.test/admin/edit/1', pretendToBeVisual: true, runScripts: 'outside-only'
    });
    const { window } = dom;
    window.scrollTo = () => {};
    window.scrollBy = () => {};
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
    if (profile !== null) window.document.body.dataset.editorProfile = profile;
    window.eval(profilesScript);
    Object.assign(window.z_editor_tiptap_config, configuration);
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

test('asides and admonitions preserve Markdown and legacy site markup in every profile', () => {
    const html = '<aside lang="en"><p>Aside</p><ul><li><p>Item</p></li></ul></aside>'
        + '<div class="admonition note" role="note"><p class="first admonition-title">Custom title</p><p>Note body</p></div>'
        + '<aside class="admonition external-module"><p class="admonition-title">Module</p><p>Details</p></aside>';
    const p = page(html, {}, 'simple');
    try {
        const editor = p.api.get('body-en');
        assert.deepEqual(Array.from(editor.getJSON().content, node => node.type), ['aside', 'admonition', 'admonition']);
        const output = editor.getHTML();
        assert.match(output, /<aside lang="en">/);
        assert.match(output, /<div class="admonition note" role="note">/);
        assert.match(output, /class="first admonition-title">Custom title/);
        assert.match(output, /<aside class="admonition external-module"/);
        editor.commands.setContent(output);
        assert.equal(editor.getHTML(), output);
        assert.equal(p.window.document.querySelector('[data-command="aside"]'), null);
    } finally { p.close(); }
});

test('aside and note toolbar toggles wrap selections, support undo and respect readonly', () => {
    const p = page('<p>First</p><p>Second</p>', { advanced: { toolbar: ['aside', 'note'] } });
    try {
        const editor = p.api.get('body-en');
        editor.commands.setTextSelection({ from: 1, to: 13 });
        const aside = p.window.document.querySelector('[data-command="aside"]');
        const note = p.window.document.querySelector('[data-command="note"]');
        assert.equal(aside.disabled, false);
        aside.click();
        assert.match(editor.getHTML(), /^<aside><p>First<\/p><p>Second<\/p><\/aside>$/);
        assert.equal(aside.getAttribute('aria-pressed'), 'true');
        aside.click();
        assert.equal(editor.getHTML(), '<p>First</p><p>Second</p>');
        p.api.configure({ labels: { note: 'Note' } });
        assert.equal(note.disabled, false);
        const before = editor.getHTML();
        note.click();
        assert.match(editor.getHTML(), /class="admonition note" role="note"><p class="first admonition-title">Note<\/p><p>First/);
        assert.equal(note.getAttribute('aria-pressed'), 'true');
        editor.commands.undo();
        assert.equal(editor.getHTML(), before);
        editor.commands.redo();
        editor.commands.setTextSelection({ from: 2, to: editor.state.doc.content.size - 2 });
        note.click();
        assert.doesNotMatch(editor.getHTML(), /<div/);
        assert.match(editor.getHTML(), /First/);
        editor.setEditable(false);
        assert.equal(aside.disabled, true);
        assert.equal(note.disabled, true);
    } finally { p.close(); }
});

test('paste retains admonition semantics without Office classes', () => {
    const html = cleanPaste('<div class="admonition note MsoNormal" role="note"><p class="first admonition-title MsoNormal">Title</p><p>Body</p></div><aside><p>Aside</p></aside>');
    assert.doesNotMatch(html, /MsoNormal/);
    const p = page(html);
    try {
        const editor = p.api.get('body-en');
        assert.equal(editor.getJSON().content[0].type, 'admonition');
        assert.equal(editor.getJSON().content[1].type, 'aside');
        assert.match(editor.getHTML(), /class="admonition-title">Title/);
    } finally { p.close(); }
});

test('setContent imports translated media markers and saves the edited value', () => {
    const p = page();
    try {
        const html = '<p>Translated</p><!-- z-media 123 {"caption":"Caption"} -->';
        assert.equal(p.api.setContent('body-en', html), true);
        assert.equal(p.api.get('body-en').getJSON().content[1].type, 'zotonicMedia');
        p.api.save();
        assert.match(p.window.document.getElementById('body-en').value, /<!-- z-media 123/);
        p.api.get('body-en').setEditable(false);
        assert.equal(p.api.setContent('body-en', '<p>Rejected</p>'), false);
    } finally { p.close(); }
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

test('paste cleanup preserves code languages but strips highlighter and Office classes', () => {
    const html = cleanPaste('<pre class="language-python highlight" id="sample"><code class="language-erlang hljs" style="font-family:monospace"><span class="token keyword">case</span> X of\n    ok -&gt; ok\nend.</code></pre><p class="language-json MsoNormal">Text</p>');
    assert.equal(html, '<pre><code class="language-erlang"><span>case</span> X of\n    ok -&gt; ok\nend.</code></pre><p>Text</p>');
    assert.equal(cleanPaste('<pre class="language-custom"><code class="hljs">x</code></pre>'),
        '<pre><code class="language-custom">x</code></pre>');
    assert.equal(cleanPaste('<pre><code class="language- hljs">plain</code></pre>'),
        '<pre><code>plain</code></pre>');
});

test('pasted code language survives the editor schema and HTML export', () => {
    const p = page();
    try {
        const editor = p.api.get('body-en');
        editor.commands.setContent(cleanPaste('<pre class="language-erlang"><code>ok.</code></pre>'));
        assert.equal(editor.getJSON().content[0].attrs.language, 'erlang');
        assert.match(editor.getHTML(), /<code class="language-erlang">ok\.<\/code>/);
    } finally { p.close(); }
});

test('code language selector targets its own block, supports undo, and is not serialized', () => {
    const p = page('<pre><code>first</code></pre><pre><code class="language-erlang">second</code></pre>');
    try {
        const editor = p.api.get('body-en');
        const selects = p.window.document.querySelectorAll('.z-tiptap-code-controls select');
        assert.equal(selects.length, 2);
        assert.equal(selects[0].value, '');
        assert.equal(selects[1].value, 'erlang');
        editor.commands.setTextSelection(1);
        selects[1].value = 'json';
        selects[1].dispatchEvent(new p.window.Event('change', { bubbles: true }));
        assert.equal(editor.getJSON().content[0].attrs.language, null);
        assert.equal(editor.getJSON().content[1].attrs.language, 'json');
        assert.equal(editor.getText(), 'first\n\nsecond');
        assert.doesNotMatch(editor.getHTML(), /select|option|z-tiptap-code/);
        editor.commands.undo();
        assert.equal(selects[1].value, 'erlang');
        editor.commands.redo();
        assert.equal(selects[1].value, 'json');
        selects[1].value = '';
        selects[1].dispatchEvent(new p.window.Event('change', { bubbles: true }));
        assert.doesNotMatch(editor.getHTML(), /language-/);
    } finally { p.close(); }
});

test('Enter inserts code newlines, including after changing the language', () => {
    const p = page('<p>Before</p><pre><code>firstsecond</code></pre>');
    try {
        const editor = p.api.get('body-en');
        const select = p.window.document.querySelector('.z-tiptap-code-controls select');
        editor.commands.setTextSelection(1);
        select.focus();
        select.value = 'erlang';
        select.dispatchEvent(new p.window.Event('change', { bubbles: true }));
        assert.equal(p.window.document.activeElement, editor.view.dom);
        assert.equal(editor.state.selection.$from.parent.type.name, 'codeBlock');
        const start = editor.state.selection.from;
        editor.commands.setTextSelection(start + 5);
        const enter = () => editor.view.dom.dispatchEvent(new p.window.KeyboardEvent('keydown', {
            key: 'Enter', code: 'Enter', keyCode: 13, bubbles: true, cancelable: true
        }));
        enter();
        assert.equal(editor.state.doc.lastChild.textContent, 'first\nsecond');
        editor.commands.setTextSelection(start + editor.state.doc.lastChild.content.size);
        enter();
        assert.equal(editor.state.doc.lastChild.textContent, 'first\nsecond\n');
        select.focus();
        select.value = 'json';
        const caret = editor.state.selection.from;
        select.dispatchEvent(new p.window.Event('change', { bubbles: true }));
        assert.equal(editor.state.selection.from, caret);
        assert.equal(p.window.document.activeElement, editor.view.dom);
        enter();
        assert.equal(editor.state.doc.lastChild.textContent, 'first\nsecond\n\n');
    } finally { p.close(); }
});

test('code languages allow profile overrides, preserve unknown values and respect readonly', () => {
    const p = page('<pre><code class="language-custom">sample</code></pre>', {
        advanced: { codeLanguages: { erlang: 'Erlang' } }
    });
    try {
        const editor = p.api.get('body-en');
        const select = p.window.document.querySelector('.z-tiptap-code-controls select');
        assert.deepEqual([...select.options].map(o => o.value), ['', 'erlang', 'custom']);
        assert.equal(select.value, 'custom');
        p.api.configure({ labels: { codeLanguage: 'Code language', plainText: 'Plain text' } });
        assert.equal(select.title, 'Code language');
        assert.equal(select.options[0].textContent, 'Plain text');
        editor.setEditable(false);
        assert.equal(select.disabled, true);
        assert.equal(select.parentElement.hidden, true);
        select.value = 'erlang';
        select.dispatchEvent(new p.window.Event('change', { bubbles: true }));
        assert.equal(editor.getJSON().content[0].attrs.language, 'custom');
    } finally { p.close(); }
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

test('image properties shortcut selects its own media and respects readonly state', async () => {
    const original = '<!-- z-media 12 {} --><!-- z-media 34 {"caption":"Second"} -->';
    const p = page(original);
    try {
        const editor = p.api.get('body-en');
        const buttons = p.window.document.querySelectorAll('.z-tiptap-media-properties');
        assert.equal(buttons.length, 2);
        assert.ok(buttons[1].getAttribute('aria-label'));
        buttons[1].click();
        assert.equal(p.events.at(-1).name, 'media_props');
        assert.equal(p.events.at(-1).args.id, '34');
        assert.equal(editor.state.selection.node.attrs.id, '34');
        p.api.save();
        assert.equal(p.window.document.querySelector('#body-en').value, original);
        assert.doesNotMatch(editor.getHTML(), /button|svg|z-tiptap-media-properties/);
        p.window.document.querySelector('#body-en').readOnly = true;
        await new Promise(resolve => setTimeout(resolve, 0));
        assert.equal(buttons[1].disabled, true);
        assert.equal(buttons[1].hidden, true);
        const count = p.events.length;
        buttons[1].click();
        assert.equal(p.events.length, count);
    } finally { p.close(); }
});

test('media figures display optional captions as text and update with undo', () => {
    const p = page('<!-- z-media 12 {"caption":"A caption"} -->');
    try {
        const editor = p.api.get('body-en');
        const figure = p.window.document.querySelector('figure.z-tiptap-media');
        assert.ok(figure);
        assert.equal(figure.querySelector('figcaption').textContent, 'A caption');
        editor.commands.setNodeSelection(0);
        for (const caption of ['', '-', '   ']) {
            editor.commands.updateAttributes('zotonicMedia', { options: { caption } });
            assert.equal(figure.querySelector('figcaption'), null);
        }
        editor.commands.updateAttributes('zotonicMedia', { options: { caption: '<img src=x onerror=alert(1)>' } });
        assert.equal(figure.querySelector('figcaption').textContent, '<img src=x onerror=alert(1)>');
        assert.equal(figure.querySelector('figcaption img'), null);
        p.api.save();
        const saved = p.window.document.querySelector('#body-en').value;
        assert.match(saved, /<!-- z-media 12/);
        assert.doesNotMatch(saved, /<figure|<figcaption|<button/);
        editor.commands.undo();
        assert.equal(figure.querySelector('figcaption').textContent, 'A caption');
    } finally { p.close(); }
});

test('late translated configuration refreshes existing editor labels without losing history', () => {
    const original = '<!-- z-media 12 {} --><p>Hello</p>';
    const p = page(original);
    try {
        const editor = p.api.get('body-en');
        p.api.configure({ labels: {
            toolbar: 'Tekstopmaak', zlink: 'Interne link invoegen',
            mediaProperties: 'Media-eigenschappen', format: 'Opmaak',
            paragraph: 'Alinea', table: 'Tabel', insertTable: 'Tabel invoegen'
        } });
        const doc = p.window.document;
        assert.equal(doc.querySelector('[data-command="zlink"]').title, 'Interne link invoegen');
        assert.equal(doc.querySelector('[data-command="zlink"]').getAttribute('aria-label'), 'Interne link invoegen');
        assert.equal(doc.querySelector('.z-tiptap-toolbar').getAttribute('aria-label'), 'Tekstopmaak');
        assert.equal(doc.querySelector('select[aria-label="Opmaak"] option').textContent, 'Alinea');
        assert.equal(doc.querySelector('select[aria-label="Tabel"]').title, 'Tabel');
        assert.equal(doc.querySelector('option[value="insertTable"]').textContent, 'Tabel invoegen');
        assert.equal(doc.querySelector('.z-tiptap-media-properties').title, 'Media-eigenschappen');
        assert.equal(doc.querySelector('.z-tiptap-media img').title, 'Media-eigenschappen');
        p.api.save();
        assert.equal(doc.querySelector('#body-en').value, original);
        editor.commands.insertContentAt(editor.state.doc.content.size, '<p>Added</p>');
        p.api.configure({ labels: { zlink: 'Link to page' } });
        assert.equal(p.api.get('body-en'), editor);
        assert.equal(editor.commands.undo(), true);
        assert.doesNotMatch(editor.getText(), /Added/);
        assert.equal(doc.querySelector('.z-tiptap-media-properties').title, 'Media-eigenschappen');
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
    const p = page('<h2>Heading</h2><table><tbody><tr><td><p>Cell</p></td></tr></tbody></table>', { simple: { toolbar: ['bold'] } }, 'simple');
    try {
        assert.equal(p.window.document.querySelectorAll('.z-tiptap-toolbar button').length, 1);
        assert.match(p.api.get('body-en').getHTML(), /<table/);
    } finally { p.close(); }
});

test('profiles inherit from the nearest ancestor with textarea overrides and normal fallback', () => {
    const p = page();
    try {
        const doc = p.window.document;
        const textarea = doc.querySelector('#body-en');
        const has = name => !!doc.querySelector('[data-command="' + name + '"]');
        assert.ok(has('strike'));
        p.api.remove();
        textarea.form.dataset.editorProfile = 'simple';
        p.api.add(textarea);
        assert.ok(has('bold'));
        assert.equal(has('zmedia'), false);
        assert.equal(doc.querySelector('.z-tiptap-toolbar select'), null);
        p.api.remove();
        textarea.dataset.editorProfile = 'normal';
        p.api.add(textarea);
        assert.ok(has('zmedia'));
        assert.equal(has('strike'), false);
        p.api.remove();
        textarea.dataset.editorProfile = 'unknown';
        p.api.add(textarea);
        assert.ok(has('zmedia'));
        assert.equal(has('strike'), false);
        p.api.remove();
        delete textarea.dataset.editorProfile;
        delete textarea.form.dataset.editorProfile;
        delete doc.body.dataset.editorProfile;
        textarea.dataset.zeditor = '{"config":"simple"}';
        p.api.add(textarea);
        assert.ok(has('zmedia'));
        assert.equal(has('strike'), false);
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

test('fullscreen save controls delegate to the original form submitters', async () => {
    const p = page();
    try {
        const doc = p.window.document;
        const form = doc.querySelector('form');
        const targets = ['save_stay', 'save_view'].map(id => {
            const button = doc.createElement('button');
            button.type = 'submit';
            button.id = id;
            button.hidden = true;
            form.append(button);
            return button;
        });
        const submissions = [];
        form.addEventListener('submit', event => {
            event.preventDefault();
            submissions.push(event.submitter.id);
            assert.match(doc.querySelector('#body-en').value, /Edited/);
        });
        p.api.configure({ labels: { save: 'Save', saveAndView: 'Save and View' } });
        p.api.get('body-en').commands.insertContent('Edited');
        assert.equal(doc.querySelector('.z-tiptap-save-actions'), null);
        doc.querySelector('[data-command="fullscreen"]').click();
        const save = doc.querySelector('[data-save-action="save_stay"]');
        const view = doc.querySelector('[data-save-action="save_view"]');
        assert.equal(save.textContent, 'Save');
        assert.equal(view.textContent, 'Save and View');
        save.click();
        view.click();
        assert.deepEqual(submissions, ['save_stay', 'save_view']);
        targets[0].disabled = true;
        await new Promise(resolve => setTimeout(resolve, 0));
        assert.equal(save.disabled, true);
        save.click();
        assert.equal(submissions.length, 2);
        doc.querySelector('[data-command="fullscreen"]').click();
        assert.equal(doc.querySelector('.z-tiptap-save-actions'), null);
        targets[1].remove();
        doc.querySelector('[data-command="fullscreen"]').click();
        assert.equal(doc.querySelector('[data-save-action="save_view"]'), null);
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
