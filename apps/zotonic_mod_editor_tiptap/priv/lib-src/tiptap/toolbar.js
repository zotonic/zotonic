// Copyright 2026 Zotonic. Apache-2.0.
import {
    createElement, Undo2, Redo2, Bold, Italic, Underline, Strikethrough,
    List, ListOrdered, IndentIncrease, IndentDecrease, Quote, Code, Link,
    Unlink, ImagePlus, Maximize, RemoveFormatting, AlignLeft, AlignCenter,
    AlignRight, Subscript, Superscript, WrapText, Minus, ExternalLink,
    ArrowLeftToLine, ArrowRightToLine
} from 'lucide';

export const defaultToolbar = [
    'undo', 'redo', '|', 'format', 'bold', 'italic', 'underline', '|',
    'bulletList', 'orderedList', 'outdent', 'indent', 'blockquote', '|',
    'link', 'unlink', 'zlink', 'zmedia', 'table', '|', 'removeFormat', 'fullscreen'
];

const commands = {
    undo: [Undo2, chain => chain.undo()],
    redo: [Redo2, chain => chain.redo()],
    bold: [Bold, chain => chain.toggleBold(), 'bold'],
    italic: [Italic, chain => chain.toggleItalic(), 'italic'],
    underline: [Underline, chain => chain.toggleUnderline(), 'underline'],
    strike: [Strikethrough, chain => chain.toggleStrike(), 'strike'],
    subscript: [Subscript, chain => chain.toggleSubscript(), 'subscript'],
    superscript: [Superscript, chain => chain.toggleSuperscript(), 'superscript'],
    bulletList: [List, chain => chain.toggleBulletList(), 'bulletList'],
    orderedList: [ListOrdered, chain => chain.toggleOrderedList(), 'orderedList'],
    indent: [IndentIncrease, chain => chain.sinkListItem('listItem')],
    outdent: [IndentDecrease, chain => chain.liftListItem('listItem')],
    blockquote: [Quote, chain => chain.toggleBlockquote(), 'blockquote'],
    codeBlock: [Code, chain => chain.toggleCodeBlock(), 'codeBlock'],
    alignLeft: [AlignLeft, chain => chain.setTextAlign('left')],
    alignCenter: [AlignCenter, chain => chain.setTextAlign('center')],
    alignRight: [AlignRight, chain => chain.setTextAlign('right')],
    ltr: [ArrowLeftToLine, chain => chain.updateAttributes('paragraph', { dir: 'ltr' }).updateAttributes('heading', { dir: 'ltr' })],
    rtl: [ArrowRightToLine, chain => chain.updateAttributes('paragraph', { dir: 'rtl' }).updateAttributes('heading', { dir: 'rtl' })],
    horizontalRule: [Minus, chain => chain.setHorizontalRule()],
    hardBreak: [WrapText, chain => chain.setHardBreak()],
    unlink: [Unlink, chain => chain.extendMarkRange('link').unsetLink()],
    removeFormat: [RemoveFormatting, chain => chain.unsetAllMarks().clearNodes()]
};
const icons = { link: Link, zlink: ExternalLink, zmedia: ImagePlus, fullscreen: Maximize };

export function toolbar(record, actions, label) {
    const { editor, config } = record;
    const bar = document.createElement('div');
    bar.className = 'z-tiptap-toolbar';
    bar.setAttribute('role', 'group');
    bar.setAttribute('aria-label', label('toolbar'));
    const updates = [];
    const controls = config.toolbar || defaultToolbar;
    for (const name of controls) {
        if (name === '|') {
            const separator = document.createElement('span');
            separator.className = 'z-tiptap-separator';
            separator.setAttribute('aria-hidden', 'true');
            bar.append(separator);
        } else if (name === 'format') {
            const select = document.createElement('select');
            select.setAttribute('aria-label', label('format'));
            select.title = label('format');
            for (const value of ['paragraph', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'codeBlock']) {
                select.add(new Option(label(value), value));
            }
            select.addEventListener('change', () => {
                const value = select.value;
                if (value.startsWith('h')) editor.chain().focus().setHeading({ level: Number(value[1]) }).run();
                else if (value === 'codeBlock') editor.chain().focus().setCodeBlock().run();
                else editor.chain().focus().setParagraph().run();
            });
            bar.append(select);
            updates.push(() => {
                select.disabled = !editor.isEditable;
                select.value = editor.isActive('heading') ? `h${editor.getAttributes('heading').level}`
                    : editor.isActive('codeBlock') ? 'codeBlock' : 'paragraph';
            });
        } else if (name === 'table') {
            const select = document.createElement('select');
            select.setAttribute('aria-label', label('table'));
            const tableCommands = {
                insertTable: chain => chain.insertTable({ rows: 3, cols: 3, withHeaderRow: true }),
                addRowAfter: chain => chain.addRowAfter(),
                addColumnAfter: chain => chain.addColumnAfter(),
                deleteRow: chain => chain.deleteRow(),
                deleteColumn: chain => chain.deleteColumn(),
                toggleHeaderRow: chain => chain.toggleHeaderRow(),
                mergeCells: chain => chain.mergeCells(),
                splitCell: chain => chain.splitCell(),
                deleteTable: chain => chain.deleteTable()
            };
            select.add(new Option(label('table'), ''));
            for (const key of Object.keys(tableCommands)) select.add(new Option(label(key), key));
            select.addEventListener('change', () => {
                tableCommands[select.value]?.(editor.chain().focus()).run();
                select.value = '';
            });
            bar.append(select);
            updates.push(() => {
                select.disabled = !editor.isEditable;
                for (const option of [...select.options].slice(1)) {
                    option.disabled = !tableCommands[option.value](editor.can().chain()).run();
                }
            });
        } else {
            const command = commands[name];
            const custom = config.buttons?.[name];
            if (!command && !actions[name] && !custom) continue;
            const button = document.createElement('button');
            button.type = 'button';
            button.title = custom?.label || label(name);
            button.setAttribute('aria-label', button.title);
            button.dataset.command = name;
            const icon = command?.[0] || icons[name] || custom?.icon;
            if (icon) button.append(createElement(icon, { width: 18, height: 18, 'aria-hidden': 'true' }));
            else button.textContent = custom.label;
            button.addEventListener('mousedown', event => event.preventDefault());
            button.addEventListener('click', () => {
                if (command) command[1](editor.chain().focus()).run();
                else if (custom) custom.run(editor);
                else actions[name]();
                update();
            });
            bar.append(button);
            updates.push(() => {
                button.disabled = name !== 'fullscreen' && (!editor.isEditable
                    || (command ? !command[1](editor.can().chain()).run() : false));
                const active = command?.[2] ? editor.isActive(command[2])
                    : name === 'fullscreen' && record.shell.classList.contains('is-fullscreen');
                if (command?.[2] || name === 'fullscreen') button.setAttribute('aria-pressed', String(active));
            });
        }
    }
    function update() { updates.forEach(fn => fn()); }
    editor.on('transaction', update);
    update();
    return bar;
}
