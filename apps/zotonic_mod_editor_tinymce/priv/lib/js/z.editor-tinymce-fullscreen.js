/* Keep normal admin save and preview actions available in TinyMCE fullscreen. */
window.z_tinymce_fullscreen = function(editor) {
    let cleanup = () => {};
    editor.on('FullscreenStateChanged', ({state}) => {
        cleanup();
        cleanup = () => {};
        if (!state) return;
        const form = editor.getElement().form;
        const header = editor.getContainer().querySelector('.tox-editor-header, .mce-toolbar-grp');
        if (!header) return;
        const actions = document.createElement('div');
        actions.className = 'z-tinymce-fullscreen-actions z-editor-fullscreen-actions';
        const close = document.createElement('button');
        close.type = 'button';
        close.className = 'btn btn-default';
        close.dataset.fullscreenClose = '';
        close.textContent = editor.translate('Close');
        close.addEventListener('click', () => editor.execCommand('mceFullScreen'));
        actions.append(close);
        const controls = document.createElement('div');
        controls.className = 'z-editor-fullscreen-controls';
        actions.append(controls);
        const observers = [];
        for (const id of ['save_stay', 'save_view']) {
            const target = Array.from(form?.elements || []).find(el => el.id === id && el.type === 'submit');
            if (!target) continue;
            const save = document.createElement('button');
            save.type = 'button';
            save.className = id === 'save_stay' ? 'btn btn-primary' : 'btn btn-default';
            save.dataset.saveAction = id;
            save.textContent = target.textContent;
            save.title = target.title;
            const update = () => {
                save.disabled = target.matches(':disabled')
                    || !!editor.mode?.isReadOnly() || !!editor.getParam('readonly');
            };
            save.addEventListener('click', () => {
                update();
                if (!save.disabled && target.isConnected && target.form === form) {
                    window.tinymce.triggerSave();
                    target.click();
                }
            });
            controls.append(save);
            const observer = new MutationObserver(update);
            observer.observe(target, {attributes: true, attributeFilter: ['disabled']});
            editor.on('SwitchMode', update);
            update();
            observers.push(() => { observer.disconnect(); editor.off('SwitchMode', update); });
        }
        const preview = form?.querySelector('[data-admin-preview="toggle"]');
        if (preview) {
            const toggle = preview.cloneNode(true);
            toggle.removeAttribute('id');
            toggle.type = 'button';
            controls.append(toggle);
        }
        if (actions.childElementCount) header.prepend(actions);
        cleanup = () => {
            observers.forEach(dispose => dispose());
            actions.remove();
        };
    });
    editor.on('remove', () => cleanup());
};
