// Copyright 2026 Zotonic. Apache-2.0.

export function safeUrl(value, image = false) {
    const url = String(value || '').trim();
    const normalized = url.replace(/[\u0000-\u0020\u007f]/g, '');
    if (!normalized || /^(?:https?:|\/|#|\?|\.\/|\.\.\/)/i.test(normalized)) return url;
    if (!image && /^(?:mailto:|tel:)/i.test(normalized)) return url;
    return /^[^:/?#]+(?:[/?#]|$)/.test(normalized) ? url : '';
}

export function mediaOptions(value) {
    const options = typeof value === 'string' ? JSON.parse(value) : value;
    if (!options || typeof options !== 'object' || Array.isArray(options)) {
        throw new Error('Invalid Zotonic media options');
    }
    return options;
}

export function importHTML(html) {
    const template = document.createElement('template');
    template.innerHTML = html;
    const walker = document.createTreeWalker(template.content, NodeFilter.SHOW_COMMENT);
    const comments = [];
    while (walker.nextNode()) comments.push(walker.currentNode);
    for (const comment of comments) {
        const match = comment.data.match(/^\s*z-media\s+(\d+)\s+([\s\S]+?)\s*$/);
        if (!match) continue;
        const img = document.createElement('img');
        img.setAttribute('data-zmedia-id', match[1]);
        img.setAttribute('data-zmedia-opts', JSON.stringify(mediaOptions(match[2])));
        comment.replaceWith(img);
    }
    return template.innerHTML;
}

export function exportHTML(editor) {
    const template = document.createElement('template');
    template.innerHTML = editor.getHTML();
    for (const img of template.content.querySelectorAll('img[data-zmedia-id]')) {
        const id = img.getAttribute('data-zmedia-id');
        const options = JSON.stringify(mediaOptions(img.getAttribute('data-zmedia-opts')))
            .replace(/-/g, '\\u002d');
        img.replaceWith(document.createComment(` z-media ${id} ${options} `));
    }
    return template.innerHTML;
}

// Office lists often consist of styled paragraphs, not semantic HTML lists.
// Reconstruct their structure before the schema discards the mso-list metadata.
export function cleanPaste(html) {
    const template = document.createElement('template');
    template.innerHTML = html;
    let stack = [];
    let previous = null;
    let listId = null;
    for (const paragraph of [...template.content.querySelectorAll('p')]) {
        const info = (paragraph.getAttribute('style') || '').match(/mso-list:\s*(l\d+)\s+level(\d+)/i);
        if (!info) { stack = []; previous = null; continue; }
        if (paragraph.previousElementSibling !== previous || info[1] !== listId) stack = [];
        listId = info[1];
        const marker = paragraph.querySelector('[style*="mso-list:Ignore"], [style*="mso-list: Ignore"]');
        const markerText = marker?.textContent.trim() || paragraph.textContent.trim().split(/\s/)[0];
        const ordered = /^(?:\d+|[a-z]+)[.)]$/i.test(markerText);
        const depth = Math.max(1, Math.min(Number(info[2]), stack.length + 1));
        stack = stack.slice(0, depth);
        if (stack[depth - 1]?.localName !== (ordered ? 'ol' : 'ul')) stack = stack.slice(0, depth - 1);
        if (stack.length < depth) {
            const list = document.createElement(ordered ? 'ol' : 'ul');
            const start = parseInt(markerText, 10);
            if (ordered && start > 1) list.setAttribute('start', start);
            if (depth > 1) stack[depth - 2].lastElementChild.append(list);
            else paragraph.before(list);
            stack.push(list);
        }
        marker?.remove();
        const item = document.createElement('li');
        item.append(...paragraph.childNodes);
        stack[depth - 1].append(item);
        previous = stack[0];
        paragraph.remove();
    }
    for (const el of template.content.querySelectorAll('[class], [style], [id]')) {
        el.removeAttribute('class');
        el.removeAttribute('id');
        // Keep semantic emphasis/alignment for the schema, discard Office typography.
        const style = el.style;
        const kept = ['font-weight', 'font-style', 'text-decoration', 'text-align']
            .map(key => style.getPropertyValue(key) ? `${key}:${style.getPropertyValue(key)}` : '')
            .filter(Boolean).join(';');
        if (kept) el.setAttribute('style', kept);
        else el.removeAttribute('style');
    }
    return template.innerHTML;
}
