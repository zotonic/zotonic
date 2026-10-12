// Copyright 2026 Zotonic. Apache-2.0.
import { Node } from '@tiptap/core';

export const Aside = Node.create({
    name: 'aside',
    group: 'block',
    content: 'block+',
    defining: true,
    parseHTML: () => [{ tag: 'aside:not(.admonition)' }],
    renderHTML: ({ HTMLAttributes }) => ['aside', HTMLAttributes, 0],
    addCommands() {
        return { toggleAside: () => ({ commands }) => commands.toggleWrap(this.name) };
    }
});

// Markdown uses a div; older site content also uses aside admonitions.
export const Admonition = Node.create({
    name: 'admonition',
    group: 'block',
    content: 'block+',
    defining: true,
    addAttributes() {
        return {
            tag: { default: 'div', rendered: false, parseHTML: el => el.localName },
            role: { default: 'note' }
        };
    },
    parseHTML: () => [{ tag: 'div.admonition' }, { tag: 'aside.admonition' }],
    renderHTML: ({ node, HTMLAttributes }) => [node.attrs.tag === 'aside' ? 'aside' : 'div', HTMLAttributes, 0],
    addCommands() {
        return {
            toggleAdmonition: (title = 'Note') => ({ editor, chain }) => {
                if (editor.isActive(this.name)) return chain().lift(this.name).run();
                return chain().wrapIn(this.name, { class: 'admonition note' })
                    .command(({ tr, dispatch }) => {
                        if (!dispatch) return true;
                        // Insert the title in the same transaction, keeping undo atomic.
                        const { $from } = tr.selection;
                        for (let depth = $from.depth; depth > 0; depth--) {
                            if ($from.node(depth).type.name === this.name) {
                                tr.insert($from.start(depth), tr.doc.type.schema.nodes.paragraph.create(
                                    { class: 'first admonition-title' }, tr.doc.type.schema.text(title)));
                                return true;
                            }
                        }
                        return false;
                    }).run();
            }
        };
    }
});
