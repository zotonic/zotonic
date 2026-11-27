/* Editor adapters share upload transport and feedback with the admin drop zones.
 * Only file drops are claimed; internal editor drags retain their native behavior.
 */
(function() {
    'use strict';
    const zones = new WeakMap();
    const messages = () => document.getElementById('fileuploader-messages').dataset;
    const filesIn = event => Array.from(event.dataTransfer?.types || []).includes('Files');

    function feedback(host) {
        let zone = zones.get(host);
        if (!zone) {
            zone = document.createElement('div');
            zone.id = 'editor-upload-' + crypto.randomUUID();
            zone.className = 'fileuploader-editor';
            const status = document.createElement('p');
            status.className = 'fileuploader-status';
            status.setAttribute('role', 'status');
            status.setAttribute('aria-live', 'polite');
            zone.append(status);
            host.after(zone);
            zones.set(host, zone);
        }
        return zone;
    }

    function editorDrop({event, host, pageId, reserve}) {
        if (!filesIn(event)) return false;
        event.preventDefault();
        event.stopPropagation();
        window.z_fileuploader.clearDragState();
        const zone = feedback(host);
        const text = messages();
        if (zone.getAttribute('aria-busy') === 'true') return true;
        const folders = Array.from(event.dataTransfer.items || []).some(item => item.webkitGetAsEntry?.()?.isDirectory);
        if (folders || !/^[1-9]\d*$/.test(String(pageId))) {
            const status = zone.querySelector('.fileuploader-status');
            status.textContent = folders ? text.folder : text.editorNoPage;
            status.classList.add('is-error');
            return true;
        }
        window.z_fileuploader.editorUpload({
            zone, files: Array.from(event.dataTransfer.files), reserve,
            options: {subject_id: Number(pageId), predicate: 'depiction',
                is_dependent: text.editorDependent !== 'false', is_published: text.editorPublished === 'true'}
        });
        return true;
    }

    function tinyMCEDrop(editor, event) {
        if (!filesIn(event)) return;
        // Read-only editors must not fall through to a surrounding page upload zone.
        if (editor.mode?.isReadOnly() || editor.getParam('readonly')) {
            event.preventDefault(); event.stopPropagation(); return;
        }
        const element = editor.getElement();
        editorDrop({event, host: editor.getContainer(),
            pageId: element.dataset.id || element.form?.dataset.id || element.form?.elements.namedItem('id')?.value,
            reserve() {
                const doc = editor.getDoc();
                let range = doc.caretRangeFromPoint?.(event.clientX, event.clientY);
                if (!range && doc.caretPositionFromPoint) {
                    const point = doc.caretPositionFromPoint(event.clientX, event.clientY);
                    if (point) {
                        range = doc.createRange();
                        range.setStart(point.offsetNode, point.offset);
                    }
                }
                if (!range || !editor.getBody().contains(range.startContainer)) return null;
                range.collapse(true);
                // TinyMCE excludes bookmark spans from serialized content. One per file
                // preserves order even when an earlier failed file is retried later.
                const markers = Array.from(event.dataTransfer.files, () => {
                    const marker = doc.createElement('span');
                    marker.setAttribute('data-mce-type', 'bookmark');
                    marker.setAttribute('data-mce-bogus', 'all');
                    marker.hidden = true;
                    range.insertNode(marker);
                    range.setStartAfter(marker);
                    return marker;
                });
                const dispose = () => markers.forEach(marker => marker.remove());
                editor.on('remove', dispose);
                return {
                    insert(id, index) {
                        const marker = markers[index];
                        if (editor.removed || editor.mode?.isReadOnly() || editor.getParam('readonly') || !marker.isConnected) return false;
                        editor.undoManager.transact(() => {
                            marker.insertAdjacentHTML('beforebegin', window.tinyMCEzMedia.toHTML(id, {align: 'block', size: 'large'}));
                            marker.remove();
                        });
                        editor.nodeChanged();
                        editor.fire('change');
                        return true;
                    },
                    dispose() { dispose(); editor.off('remove', dispose); }
                };
            }
        });
    }

    Object.assign(window.z_fileuploader, {editorDrop, tinyMCEDrop, filesIn});
})();
