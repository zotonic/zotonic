// Copyright 2026 Marc Worrell. Apache-2.0.
import CodeBlock from '@tiptap/extension-code-block';
import { TextSelection } from '@tiptap/pm/state';

export function codeBlock(languages = {}, label) {
    return CodeBlock.extend({
        addNodeView() {
            return ({ node, editor, getPos }) => {
                const dom = document.createElement('div');
                dom.className = 'z-tiptap-code-block';
                const controls = document.createElement('div');
                controls.className = 'z-tiptap-code-controls';
                controls.contentEditable = 'false';
                const select = document.createElement('select');
                const pre = document.createElement('pre');
                const code = document.createElement('code');
                pre.append(code);
                controls.append(select);
                dom.append(controls, pre);

                const refresh = () => {
                    select.title = label('codeLanguage');
                    select.setAttribute('aria-label', select.title);
                    select.disabled = !editor.isEditable;
                    controls.hidden = !editor.isEditable;
                    select.options[0].textContent = label('plainText');
                };
                const render = () => {
                    const language = node.attrs.language || '';
                    select.replaceChildren(new Option(label('plainText'), ''));
                    for (const [value, name] of Object.entries(languages)) {
                        if (value) select.add(new Option(name, value));
                    }
                    if (language && !Object.hasOwn(languages, language)) {
                        select.add(new Option(language, language));
                    }
                    select.value = language;
                    if (language) code.setAttribute('class', 'language-' + language);
                    else code.removeAttribute('class');
                    for (const name of ['class', 'title', 'lang', 'dir']) {
                        if (node.attrs[name]) pre.setAttribute(name, node.attrs[name]);
                        else pre.removeAttribute(name);
                    }
                    refresh();
                };
                const change = () => {
                    const pos = getPos();
                    if (!editor.isEditable || pos === undefined) return;
                    const tr = editor.state.tr.setNodeMarkup(pos, undefined, {
                        ...node.attrs, language: select.value || null
                    });
                    // Keep the caret in this block, even when its selector was clicked from elsewhere.
                    if (tr.selection.from < pos + 1 || tr.selection.to > pos + node.nodeSize - 1) {
                        tr.setSelection(TextSelection.create(tr.doc, pos + 1));
                    }
                    editor.view.dispatch(tr);
                    editor.view.focus();
                };
                select.addEventListener('change', change);
                editor.on('transaction', refresh);
                editor.on('update', refresh);
                render();
                return {
                    dom,
                    contentDOM: code,
                    stopEvent: event => controls.contains(event.target),
                    ignoreMutation: mutation => mutation.type !== 'selection'
                        && (!code.contains(mutation.target)
                            || (mutation.type === 'attributes' && mutation.target === code)),
                    update(updated) {
                        if (updated.type !== node.type) return false;
                        const changed = updated.attrs !== node.attrs;
                        node = updated;
                        if (changed) render();
                        return true;
                    },
                    destroy() {
                        select.removeEventListener('change', change);
                        editor.off('transaction', refresh);
                        editor.off('update', refresh);
                    }
                };
            };
        }
    });
}
