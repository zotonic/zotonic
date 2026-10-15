/* Host URL preferences are local to this browser/origin. No executable or
 * command supplied by the browser is ever sent to the server. */
(function (root) {
    'use strict';
    const storageKey = 'zotonic.development.editor.v1';
    const defaults = {mode: 'auto', local: '', preset: '', url: '', server_root: '', host_root: ''};
    const presets = {
        vscode: 'vscode://file{path}:{line}:{column}',
        vscodium: 'vscodium://file{path}:{line}:{column}',
        sublime: 'subl://open?url=file://{path}&line={line}&column={column}'
    };
    const normalize = path => path.replaceAll('\\', '/').replace(/\/+$/, '');
    function mapPath(file, from, to) {
        const path = file.replaceAll('\\', '/');
        if (path.split('/').some(part => part === '..' || part === '.')) throw new Error('path_mapping');
        if (!from && !to) return path;
        const source = normalize(from);
        const target = normalize(to);
        if (!source || !target || !path.startsWith(source + '/')) throw new Error('path_mapping');
        return target + path.slice(source.length);
    }
    function editorUrl(settings, file, line = 1, column = 1) {
        const path = mapPath(file, settings.server_root, settings.host_root);
        const pattern = settings.preset === 'custom' ? settings.url : presets[settings.preset];
        // Only external application protocols. Never execute script/data or navigate
        // to a web endpoint supplied through localStorage.
        const scheme = pattern && pattern.match(/^([a-z][a-z0-9+.-]*):\/\//i);
        if (!scheme || /^(javascript|data|vbscript|file|http|https|blob|about)$/i.test(scheme[1]) ||
            !/\{(file|path)\}/.test(pattern)) throw new Error('url_pattern');
        const values = {
            file: encodeURIComponent(path),
            path: (/^[a-z]:\//i.test(path) ? '/' + path : path).split('/').map(encodeURIComponent).join('/').replace(/^\/([A-Z])%3A/i, '/$1:'),
            line: String(Math.max(1, Math.trunc(Number(line) || 1))),
            column: String(Math.max(1, Math.trunc(Number(column) || 1)))
        };
        return pattern.replace(/\{(file|path|line|column)\}/g, (_, name) => values[name]);
    }
    function read() {
        try {
            const value = JSON.parse(root.localStorage.getItem(storageKey) || '{}');
            return Object.fromEntries(Object.entries(defaults).map(([key, fallback]) =>
                [key, typeof value?.[key] === 'string' ? value[key] : fallback]));
        }
        catch (_) { return {...defaults}; }
    }
    async function call(path, payload) {
        await root.cotonic.ready;
        const response = await root.cotonic.broker.call('bridge/origin/model/development/' + path, payload);
        if (response.payload.status === 'error') throw new Error(response.payload.error || 'editor_error');
        return response.payload.result;
    }
    let detection;
    function detect() {
        if (!detection) detection = call('get/editor').catch(error => { detection = undefined; throw error; });
        return detection;
    }
    function validate(settings) {
        if (!['auto', 'local', 'browser'].includes(settings.mode)) throw new Error('mode');
        if (settings.mode === 'local' && !settings.local) throw new Error('choose_editor');
        if (settings.mode === 'browser') {
            if (Boolean(settings.server_root) !== Boolean(settings.host_root)) throw new Error('path_mapping');
            const sample = settings.server_root ? normalize(settings.server_root) + '/test.tpl' : '/test.tpl';
            editorUrl(settings, sample);
        }
    }
    function open(settings, info, file, line, column, source) {
        validate(settings);
        if (settings.mode === 'browser') {
            // Called synchronously from the click handler to retain user activation.
            root.location.href = editorUrl(settings, file, line, column);
            return Promise.resolve();
        }
        if (info.browser_only || !info.local_allowed) throw new Error('browser_only');
        return call('post/editor/open', {...(source ? {source} : {}), file, line, column, editor: settings.mode === 'auto' ? 'auto' : settings.local});
    }
    async function settings(form) {
        if (!form || form.dataset.initialized) return;
        form.dataset.initialized = 'true';
        const status = form.querySelector('[data-editor-status]');
        const summary = form.parentElement.querySelector('[data-editor-detection]');
        try {
            const info = await detect();
            const saved = read();
            if (saved.mode !== 'browser') {
                saved.local = info.editor_id === 'auto' ? '' : info.editor_id;
                saved.mode = saved.local ? 'local' : 'auto';
            }
            if (info.browser_only || !info.local_allowed) {
                saved.mode = 'browser';
                Array.from(form.elements.mode.options).forEach(option => { option.disabled = option.value !== 'browser'; });
            }
            info.candidates.forEach(candidate => {
                const option = new Option(candidate.label, candidate.id);
                option.disabled = !candidate.available;
                form.elements.local.add(option);
            });
            Object.keys(defaults).forEach(key => { form.elements[key].value = saved[key]; });
            if (!form.elements.local.value && info.selected) form.elements.local.value = info.selected.id;
            const showSummary = () => { summary.textContent = info.browser_only || !info.local_allowed ? form.dataset.container
                : !info.selected ? form.dataset.choose
                : !info.selected.available ? form.dataset.unavailable
                : info.selected.label + ' (' + info.selected.reason + ')'; };
            showSummary();
            const update = () => {
                const browser = form.elements.mode.value === 'browser';
                form.querySelector('[data-editor-browser]').hidden = !browser;
                form.querySelector('[data-editor-local]').hidden = form.elements.mode.value !== 'local';
                form.querySelector('[data-editor-custom]').hidden = form.elements.preset.value !== 'custom';
            };
            const values = () => Object.fromEntries(Object.keys(defaults).map(key => [key, form.elements[key].value.trim()]));
            form.addEventListener('change', update);
            form.addEventListener('submit', async event => {
                event.preventDefault();
                try {
                    const value = values();
                    validate(value);
                    if (value.mode !== 'browser') {
                        await call('post/editor/preference', {editor: value.mode === 'auto' ? 'auto' : value.local});
                        detection = undefined;
                        Object.assign(info, await detect());
                        showSummary();
                    }
                    root.localStorage.setItem(storageKey, JSON.stringify({
                        ...value, mode: value.mode === 'browser' ? 'browser' : 'auto', local: ''
                    }));
                    status.textContent = form.dataset.saved;
                } catch (error) { status.textContent = form.dataset.failed + ' (' + error.message + ')'; }
            });
            form.querySelector('[data-editor-test]').addEventListener('click', () => {
                try {
                    status.textContent = '';
                    open(values(), info, info.test_file, 1, 1)
                        .then(() => { status.textContent = form.dataset.openRequested; })
                        .catch(error => { status.textContent = form.dataset.openFailed + ' (' + error.message + ')'; });
                } catch (error) { status.textContent = form.dataset.openFailed + ' (' + error.message + ')'; }
            });
            form.querySelector('fieldset').disabled = false;
            update();
        } catch (error) { summary.textContent = form.dataset.failed + ' (' + error.message + ')'; }
    }
    let info;
    function prepare(button) {
        const show = allowed => {
            button.hidden = !allowed;
            const fallback = button.parentElement.querySelector('[data-editor-fallback]');
            if (fallback) fallback.hidden = allowed;
        };
        const value = read();
        if (value.mode === 'browser') {
            try { editorUrl(value, button.dataset.editorFile); show(true); }
            catch (_) { show(false); }
            return;
        }
        detect().then(result => {
            info = result;
            show(!result.browser_only && result.local_allowed && Boolean(result.selected?.available));
        }).catch(() => show(false));
    }
    if (root.document) {
        root.document.addEventListener('click', event => {
            const button = event.target.closest('[data-editor-file]');
            if (!button) return;
            event.preventDefault();
            const status = button.parentElement.querySelector('[role="status"]');
            const fail = error => { status.hidden = false; status.textContent = button.dataset.editorError + ' (' + error.message + ')'; };
            try {
                status.hidden = true;
                status.textContent = '';
                const value = read();
                if (value.mode !== 'browser') { value.mode = 'auto'; value.local = ''; }
                if (value.mode !== 'browser' && !info) throw new Error('detection_pending');
                open(value, info, button.dataset.editorFile, Number(button.dataset.editorLine || 1), Number(button.dataset.editorColumn || 1), button.dataset.editorSource).catch(fail);
            } catch (error) { fail(error); }
        });
    }
    root.zotonicDevelopmentEditor = {settings, prepare, editorUrl, mapPath};
    if (typeof module !== 'undefined') module.exports = {editorUrl, mapPath, validate};
})(typeof window === 'undefined' ? globalThis : window);
