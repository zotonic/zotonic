// Copyright 2026 Zotonic. Apache-2.0.
import { Editor, Extension, Node, Mark } from '@tiptap/core';
import { NodeSelection } from '@tiptap/pm/state';
import { importHTML, exportHTML, cleanPaste, safeUrl } from './content.js';
import { extensions, rememberSelection, restoreSelection } from './extensions.js';
import { toolbar } from './toolbar.js';

function install() {
    const records = new Map();
    const pending = new Set();
    const selector = 'textarea.z_editor-init, textarea.z_editor, textarea.tinymce-init, textarea.do_zeditor';
    let settings = { labels: {}, mediaPreviewUrl: '/admin/media/preview/__ID__' };
    let linkRequest;
    let mediaRequest;
    let propsRequest;
    let fullscreen;
    let sequence = 0;
    const label = key => settings.labels[key] || key;
    const observer = window.IntersectionObserver ? new IntersectionObserver(entries => {
        entries.filter(entry => entry.isIntersecting).forEach(entry => mount(entry.target));
    }, { rootMargin: '200px' }) : null;

    function roots(root) {
        if (!root) return [document];
        if (typeof root === 'string') return [...document.querySelectorAll(root)];
        if (root.nodeType) return [root];
        return Array.from(root);
    }

    function fields(root) {
        return [...new Set(roots(root).flatMap(el => [
            ...(el.matches?.(selector) ? [el] : []), ...el.querySelectorAll(selector)
        ]))];
    }

    function selected(root) {
        const containers = roots(root);
        return [...records.values()].filter(record => containers.some(el => el === record.textarea || el.contains(record.textarea)));
    }

    function profile(textarea) {
        let data = {};
        if (textarea.dataset.zeditor) data = JSON.parse(textarea.dataset.zeditor);
        else if (window.jQuery?.fn.metadata) data = window.jQuery(textarea).metadata('zeditor') || {};
        const key = (data.config || textarea.name || 'default').split('$')[0];
        const profiles = window.z_editor_tiptap_config || {};
        return { ...profiles.default, ...profiles[key] };
    }

    function live(record) {
        return record && records.get(record.textarea) === record && record.textarea.isConnected && !record.editor.isDestroyed;
    }

    function signal(record) {
        record.dirty = true;
        record.shell.querySelector('.z-tiptap-error')?.remove();
        record.editor.view.dom.removeAttribute('aria-invalid');
        const form = record.textarea.form;
        if (window.jQuery && form) window.jQuery(form).trigger('z:editorChange');
        else record.textarea.dispatchEvent(new CustomEvent('z:editorChange', { bubbles: true }));
    }

    function save(record) {
        if (record.dirty) record.textarea.value = exportHTML(record.editor);
    }

    function context(record) {
        const textarea = record.textarea;
        return {
            rsc_id: textarea.dataset.id || textarea.form?.dataset.id || textarea.form?.elements.namedItem('id')?.value,
            language: textarea.getAttribute('lang') || textarea.closest('[lang]')?.getAttribute('lang')
        };
    }

    function selectLink(record) {
        if (!live(record)) return;
        rememberSelection(record.editor);
        linkRequest = record;
        window.z_event('zlink', context(record));
    }

    function selectMedia(record) {
        if (!live(record)) return;
        if (record.editor.state.selection instanceof NodeSelection
            && record.editor.state.selection.node.type.name === 'zotonicMedia') {
            mediaProperties(record.editor);
            return;
        }
        rememberSelection(record.editor);
        mediaRequest = record;
        window.z_event('zmedia', { ...context(record), is_zmedia: 1 });
    }

    function mediaProperties(editor) {
        const record = [...records.values()].find(item => item.editor === editor);
        if (!live(record) || !editor.isActive('zotonicMedia')) return;
        rememberSelection(editor);
        propsRequest = { record, token: String(++sequence) };
        const { id, options } = editor.getAttributes('zotonicMedia');
        window.z_notify('media_props', {
            z_delegate: 'mod_editor_tiptap', id,
            options: JSON.stringify(options), request_id: propsRequest.token
        });
    }

    function linkDone(value) {
        const record = linkRequest;
        linkRequest = null;
        if (!live(record) || !record.editor.isEditable) return;
        const href = safeUrl(value.url_language || value.url);
        if (!href) return;
        const decoder = document.createElement('textarea');
        decoder.innerHTML = value.title_language || value.title || href;
        restoreSelection(record.editor);
        const chain = record.editor.chain().focus();
        if (record.editor.state.selection.empty) {
            chain.insertContent({ type: 'text', text: decoder.value, marks: [{ type: 'link', attrs: { href } }] }).run();
        } else chain.setLink({ href }).run();
    }

    function mediaDone(value) {
        const record = mediaRequest;
        mediaRequest = null;
        if (!live(record) || !record.editor.isEditable || !/^\d+$/.test(String(value.object_id))) return;
        restoreSelection(record.editor);
        record.editor.chain().focus().insertContent({
            type: 'zotonicMedia',
            attrs: {
                id: String(value.object_id),
                options: { align: value.is_document ? 'left' : 'block', size: value.is_document ? 'small' : 'large' }
            }
        }).run();
    }

    function propertyForm(event) {
        const form = event.target.closest?.('form[data-tiptap-media-request]');
        if (!form) return;
        const remove = event.type === 'click' && event.target.closest('[name="delete"]');
        if (event.type !== 'submit' && !remove) return;
        event.preventDefault();
        event.stopImmediatePropagation();
        if (!propsRequest || form.dataset.tiptapMediaRequest !== propsRequest.token) return;
        const { record } = propsRequest;
        propsRequest = null;
        if (!live(record) || !record.editor.isEditable) { window.z_dialog_close(); return; }
        const editor = record.editor;
        restoreSelection(editor);
        if (!editor.isActive('zotonicMedia')) return;
        if (remove) editor.chain().focus().deleteSelection().run();
        else {
            const data = new FormData(form);
            const options = { ...editor.getAttributes('zotonicMedia').options };
            for (const key of ['caption', 'align', 'size', 'crop', 'class', 'link', 'link_new', 'link_url']) {
                if (form.elements.namedItem(key)) options[key] = data.get(key) || '';
            }
            options.link_url = safeUrl(options.link_url);
            editor.chain().focus().updateAttributes('zotonicMedia', { options }).run();
        }
        window.z_dialog_close();
    }

    function editLink(record) {
        if (record.panel) { record.panel.remove(); record.panel = null; }
        rememberSelection(record.editor);
        const panel = document.createElement('div');
        panel.className = 'z-tiptap-link-panel';
        panel.setAttribute('role', 'group');
        panel.setAttribute('aria-label', label('link'));
        const input = document.createElement('input');
        input.type = 'text';
        input.setAttribute('aria-label', label('url'));
        input.placeholder = label('url');
        input.value = record.editor.getAttributes('link').href || '';
        const close = () => { panel.remove(); record.panel = null; restoreSelection(record.editor); record.editor.commands.focus(); };
        const apply = () => {
            const href = safeUrl(input.value);
            if (input.value.trim() && !href) {
                input.setCustomValidity(label('invalidUrl')); input.reportValidity(); return;
            }
            restoreSelection(record.editor);
            const chain = record.editor.chain().focus().extendMarkRange('link');
            if (!href) chain.unsetLink().run();
            else if (record.editor.state.selection.empty && !record.editor.isActive('link')) {
                chain.insertContent({ type: 'text', text: href, marks: [{ type: 'link', attrs: { href } }] }).run();
            } else chain.setLink({ href }).run();
            panel.remove(); record.panel = null;
        };
        input.addEventListener('input', () => input.setCustomValidity(''));
        input.addEventListener('keydown', event => {
            if (event.key === 'Enter') { event.preventDefault(); apply(); }
            if (event.key === 'Escape') { event.preventDefault(); close(); }
        });
        panel.append(input);
        for (const [key, action] of [['apply', apply], ['cancel', close]]) {
            const button = document.createElement('button');
            button.type = 'button'; button.textContent = label(key);
            button.addEventListener('click', action); panel.append(button);
        }
        record.toolbar.after(panel);
        record.panel = panel;
        input.focus();
    }

    function toggleFullscreen(record) {
        if (fullscreen) {
            const previous = fullscreen;
            previous.shell.classList.remove('is-fullscreen');
            previous.toolbar.querySelector('[data-command="fullscreen"]')?.setAttribute('aria-pressed', 'false');
            document.body.classList.remove('z-tiptap-fullscreen');
            fullscreen = null;
            if (live(previous)) previous.editor.view.focus();
            window.scrollTo(previous.scrollX, previous.scrollY);
            if (previous === record) return;
        }
        record.scrollX = window.scrollX;
        record.scrollY = window.scrollY;
        fullscreen = record;
        record.shell.classList.add('is-fullscreen');
        record.toolbar.querySelector('[data-command="fullscreen"]')?.setAttribute('aria-pressed', 'true');
        document.body.classList.add('z-tiptap-fullscreen');
        record.editor.view.focus();
    }

    function mount(textarea) {
        if (records.has(textarea) || !textarea.isConnected || !textarea.getClientRects().length) return;
        observer?.unobserve(textarea);
        pending.delete(textarea);
        let editor;
        const shell = document.createElement('div');
        shell.className = 'z-tiptap';
        try {
            const config = profile(textarea);
            textarea.id ||= `z-tiptap-${++sequence}`;
            const content = document.createElement('div');
            content.className = 'z-tiptap-content';
            shell.append(content);
            textarea.after(shell);
            const record = { textarea, shell, config, dirty: false, hidden: textarea.hidden, required: textarea.required };
            const extra = typeof config.extensions === 'function' ? config.extensions({ Editor, Extension, Node, Mark }) : config.extensions || [];
            editor = new Editor({
                element: content,
                injectCSS: false,
                extensions: [...extensions({ previewUrl: settings.mediaPreviewUrl, openProperties: mediaProperties, mediaLabel: label('mediaProperties') }), ...extra],
                content: importHTML(textarea.value),
                editable: !textarea.disabled && !textarea.readOnly,
                editorProps: {
                    attributes: {
                        id: `${textarea.id}-editor`,
                        class: 'z-tiptap-body', role: 'textbox', 'aria-multiline': 'true',
                        'aria-label': textarea.getAttribute('aria-label') || textarea.labels?.[0]?.textContent.trim() || textarea.name,
                        'aria-required': String(textarea.required),
                        lang: textarea.getAttribute('lang') || '', dir: textarea.getAttribute('dir') || 'auto'
                    },
                    transformPastedHTML: config.transformPastedHTML || cleanPaste
                },
                onUpdate: () => signal(record)
            });
            record.editor = editor;
            records.set(textarea, record);
            record.toolbar = toolbar(record, {
                link: () => editLink(record), zlink: () => selectLink(record),
                zmedia: () => selectMedia(record), fullscreen: () => toggleFullscreen(record)
            }, label);
            shell.prepend(record.toolbar);
            textarea.hidden = true;
            textarea.dataset.tiptapInstalled = 'true';
            textarea.required = false;
            textarea.classList.remove('z_editor-init', 'tinymce-init');
            textarea.classList.add('z_editor', 'z_editor-installed');
            record.attributes = new MutationObserver(() => {
                editor.setEditable(!textarea.disabled && !textarea.readOnly, false);
                editor.view.dispatch(editor.state.tr);
            });
            record.attributes.observe(textarea, { attributes: true, attributeFilter: ['disabled', 'readonly'] });
            config.onCreate?.(editor, textarea);
        } catch (error) {
            const record = records.get(textarea);
            if (record) {
                record.attributes?.disconnect();
                textarea.hidden = record.hidden;
                textarea.required = record.required;
                delete textarea.dataset.tiptapInstalled;
                textarea.classList.remove('z_editor-installed');
            }
            editor?.destroy();
            records.delete(textarea);
            shell.remove();
            console.error('Could not initialize Zotonic Tiptap editor', error);
        }
    }

    function add(root) {
        for (const textarea of fields(root)) {
            if (records.has(textarea) || pending.has(textarea)) continue;
            pending.add(textarea);
            if (observer) observer.observe(textarea);
            else mount(textarea);
        }
        // Initialize fields already on screen synchronously, including newly shown tabs.
        for (const textarea of pending) {
            if (textarea.getClientRects().length) {
                const rect = textarea.getBoundingClientRect();
                if (rect.top < window.innerHeight + 200 && rect.bottom > -200) mount(textarea);
            }
        }
    }

    function removeRecord(record) {
        save(record);
        if (fullscreen === record) toggleFullscreen(record);
        if (linkRequest === record) linkRequest = null;
        if (mediaRequest === record) mediaRequest = null;
        if (propsRequest?.record === record) propsRequest = null;
        record.attributes.disconnect();
        record.editor.destroy();
        record.shell.remove();
        record.textarea.hidden = record.hidden;
        delete record.textarea.dataset.tiptapInstalled;
        record.textarea.required = record.required;
        record.textarea.classList.remove('z_editor-installed');
        records.delete(record.textarea);
    }

    const api = {
        init: () => add(document), add,
        save: root => selected(root).forEach(save),
        remove(root) {
            selected(root).forEach(removeRecord);
            for (const textarea of fields(root)) { observer?.unobserve(textarea); pending.delete(textarea); }
        },
        get: id => [...records.values()].find(record => record.textarea.id === id)?.editor,
        configure: options => { settings = { ...settings, ...options, labels: { ...settings.labels, ...options.labels } }; },
        linkDone, mediaDone,
        Editor, Extension, Node, Mark
    };
    window.z_editor_tiptap = api;
    window.z_editor = api;
    document.addEventListener('submit', propertyForm, true);
    document.addEventListener('click', propertyForm, true);
    document.addEventListener('submit', event => {
        for (const record of selected(event.target)) {
            save(record);
            if (record.required && record.editor.isEditable && record.editor.isEmpty) {
                event.preventDefault(); event.stopImmediatePropagation();
                if (!record.shell.querySelector('.z-tiptap-error')) {
                    const message = document.createElement('div');
                    message.className = 'z-tiptap-error';
                    message.setAttribute('role', 'alert');
                    message.textContent = label('required');
                    record.shell.append(message);
                }
                record.editor.view.dom.setAttribute('aria-invalid', 'true');
                record.editor.commands.focus();
                return;
            }
        }
    }, true);
    document.addEventListener('reset', event => {
        const form = event.target;
        setTimeout(() => selected(form).forEach(record => {
            record.editor.commands.setContent(importHTML(record.textarea.value), { emitUpdate: false });
            record.dirty = false;
        }), 0);
    });
    document.addEventListener('keydown', event => {
        if (event.key === 'Escape' && fullscreen && fullscreen.shell.contains(event.target)
            && !event.target.closest('.z-tiptap-link-panel')) {
            event.preventDefault(); event.stopPropagation(); toggleFullscreen(fullscreen);
        }
    }, true);
    document.addEventListener('click', event => {
        const labelElement = event.target.closest('label[for]');
        if (!labelElement) return;
        const record = records.get(document.getElementById(labelElement.htmlFor));
        if (record) { event.preventDefault(); record.editor.commands.focus(); }
    });
    // Clean up records after AJAX replacement, and discover textareas inserted by templates.
    new MutationObserver(mutations => {
        for (const record of records.values()) {
            if (!record.textarea.isConnected || !record.shell.isConnected) removeRecord(record);
        }
        for (const textarea of pending) {
            if (!textarea.isConnected) { observer?.unobserve(textarea); pending.delete(textarea); }
        }
        for (const mutation of mutations) for (const node of mutation.addedNodes) {
            if (node.nodeType === 1 && (node.matches(selector) || node.querySelector(selector))) add(node);
        }
    }).observe(document.documentElement, { childList: true, subtree: true });
}

if (!window.z_editor_tiptap) install();
