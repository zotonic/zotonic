// Copyright 2026 Zotonic. Apache-2.0.
import { Extension, Node, mergeAttributes } from '@tiptap/core';
import StarterKit from '@tiptap/starter-kit';
import Image from '@tiptap/extension-image';
import { TableKit } from '@tiptap/extension-table';
import TextAlign from '@tiptap/extension-text-align';
import Subscript from '@tiptap/extension-subscript';
import Superscript from '@tiptap/extension-superscript';
import { Plugin, PluginKey, NodeSelection } from '@tiptap/pm/state';
import { mediaOptions, safeUrl } from './content.js';

const bookmarkKey = new PluginKey('zotonicDialogSelection');

// Map dialog selections through subsequent transactions instead of storing DOM ranges.
export const DialogSelection = Extension.create({
    name: 'zotonicDialogSelection',
    addProseMirrorPlugins() {
        return [new Plugin({
            key: bookmarkKey,
            state: {
                init: () => null,
                apply(tr, bookmark) {
                    const value = tr.getMeta(bookmarkKey);
                    return value !== undefined ? value : bookmark?.map(tr.mapping) || null;
                }
            }
        })];
    }
});

export function rememberSelection(editor) {
    editor.view.dispatch(editor.state.tr.setMeta(bookmarkKey, editor.state.selection.getBookmark()));
}

export function restoreSelection(editor) {
    const bookmark = bookmarkKey.getState(editor.state);
    if (!bookmark) return;
    editor.view.dispatch(editor.state.tr.setSelection(bookmark.resolve(editor.state.doc))
        .setMeta(bookmarkKey, null));
}

const ZotonicAttributes = Extension.create({
    name: 'zotonicAttributes',
    addGlobalAttributes() {
        return [{
            types: ['paragraph', 'heading', 'blockquote', 'bulletList', 'orderedList',
                'listItem', 'table', 'tableRow', 'tableCell', 'tableHeader', 'image', 'codeBlock', 'link'],
            attributes: {
                class: { default: null },
                title: { default: null },
                lang: { default: null },
                dir: {
                    default: null,
                    parseHTML: el => ['ltr', 'rtl', 'auto'].includes(el.getAttribute('dir')) ? el.getAttribute('dir') : null
                }
            }
        }];
    }
});

const SafeImage = Image.extend({
    addAttributes() {
        return {
            ...this.parent(),
            src: {
                default: null,
                parseHTML: el => safeUrl(el.getAttribute('src'), true),
                renderHTML: attrs => ({ src: safeUrl(attrs.src, true) })
            }
        };
    },
    parseHTML() {
        return [{ tag: 'img[src]:not([data-zmedia-id])' }];
    }
});

function zotonicMedia(previewUrl, openProperties, label) {
    return Node.create({
        name: 'zotonicMedia',
        group: 'block',
        atom: true,
        draggable: true,
        priority: 200,
        addAttributes() {
            return {
                id: {
                    default: null,
                    parseHTML: el => el.getAttribute('data-zmedia-id'),
                    renderHTML: attrs => ({ 'data-zmedia-id': attrs.id })
                },
                options: {
                    default: {},
                    parseHTML: el => mediaOptions(el.getAttribute('data-zmedia-opts') || '{}'),
                    renderHTML: attrs => ({ 'data-zmedia-opts': JSON.stringify(attrs.options) })
                }
            };
        },
        parseHTML() {
            return [{ tag: 'img[data-zmedia-id]', getAttrs: el => /^\d+$/.test(el.getAttribute('data-zmedia-id')) ? null : false }];
        },
        renderHTML({ HTMLAttributes }) {
            return ['img', mergeAttributes(HTMLAttributes)];
        },
        addNodeView() {
            return ({ node, editor, getPos }) => {
                const dom = document.createElement('img');
                function render(current) {
                    const { id, options } = current.attrs;
                    const align = ['left', 'right'].includes(options.align) ? options.align : 'block';
                    const size = ['small', 'middle'].includes(options.size) ? options.size : 'large';
                    dom.className = `z-tiptap-media z-tiptap-media-${align} z-tiptap-media-${size}`;
                    dom.src = previewUrl.replace('__ID__', encodeURIComponent(id));
                    dom.alt = options.caption && options.caption !== '-' ? options.caption : label;
                    dom.title = label;
                    dom.draggable = true;
                    dom.dataset.zmediaId = id;
                }
                const edit = event => {
                    if (!editor.isEditable) return;
                    event.preventDefault();
                    const pos = getPos();
                    if (pos === undefined) return;
                    editor.view.dispatch(editor.state.tr.setSelection(NodeSelection.create(editor.state.doc, pos)));
                    openProperties(editor);
                };
                dom.addEventListener('dblclick', edit);
                render(node);
                return {
                    dom,
                    update(updated) {
                        if (updated.type !== node.type) return false;
                        node = updated;
                        render(node);
                        return true;
                    },
                    destroy() { dom.removeEventListener('dblclick', edit); }
                };
            };
        }
    });
}

export function extensions({ previewUrl, openProperties, mediaLabel }) {
    return [
        StarterKit.configure({
            trailingNode: false,
            heading: { levels: [1, 2, 3, 4, 5, 6] },
            link: { openOnClick: false, autolink: false, HTMLAttributes: { target: null, rel: null } }
        }),
        SafeImage,
        TableKit.configure({ table: { resizable: false } }),
        TextAlign.configure({ types: ['heading', 'paragraph'] }),
        Subscript, Superscript, ZotonicAttributes, DialogSelection,
        zotonicMedia(previewUrl, openProperties, mediaLabel)
    ];
}
