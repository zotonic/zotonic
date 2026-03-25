<div class="template-debug">
    <div class="template-debug-filename">
        <button id="template-debug-back" type="button" class="template-debug-back" title="{_ Back _}" aria-label="{_ Back _}">←</button>
        <div id="template-debug-parents" class="template-debug-dropdown template-debug-nav"></div>
        <div class="template-debug-filename-path">
            <tt>{{ template_file|escape }}</tt>
        </div>
    </div>
    <div class="template-debug-source">
        {{ template_html }}
    </div>
    <div class="template-debug-data" id="overlay-development_debug">
        <div class="alert alert-info" style="display: none">
            {_ Maximum tracing time reached, debugging stopped. _}
            <button id="template-debug-start" class="btn btn-xs btn-primary">
                {_ Click to restart _}
            </button>
        </div>
        <div class="template-debug-data-options">
            <label class="checkbox">
                <input type="checkbox" value="all">
                {_ Debug all sessions _}
            </label>
            <p class="help-block">
                <small>
                    {_ Uncheck to debug only your current session. _}
                    {_ Debugging all sessions can result in a large amount of debug data. _}
                </small>
            </p>
            <div id="overlay-development_trace-restart" style="display: none">
                <button id="template-debug-restart" class="btn btn-xs btn-primary">
                    {_ Reload the page _}
                </button>
                <p class="help-block">
                    <small>{_ Reload the page that we are tracing to see the debug data. _}</small>
                </p>
            </div>
        </div>
        <div id="template-debug-data">
            <!-- Debug data will be loaded here -->
            <p class="help-block">
                {_ Check debug points in the template source to see debug data here. _}
            </p>
        </div>
    </div>
</div>

{% wire name="template_debug_enable"
        postback={template_debug_enable template=template_file}
        delegate=`mod_development`
%}
{% wire name="template_view"
        postback={template_view}
        delegate=`mod_development`
%}

{% javascript %}
    let is_debug_trace_enabled = false;
    const overlayDebug = document.getElementById('overlay-development_debug');
    const debugData = document.getElementById('template-debug-data');
    const debugRestart = document.getElementById('overlay-development_trace-restart');
    const debugRestartButton = document.getElementById('template-debug-restart');
    const debugStartButton = document.getElementById('template-debug-start');
    const debugAlert = overlayDebug.querySelector(':scope > .alert');
    const currentTemplateFile = '{{ template_file|escapejs }}';
    const templateBackButton = document.getElementById('template-debug-back');
    const templateParentsDropdown = document.getElementById('template-debug-parents');
    const developmentCssHref = '{% lib_url "css/development.css" %}';
    const developmentCssFile = 'css/development.css';
    const overlayHistoryState = window.__zTemplateOverlayHistoryState || (window.__zTemplateOverlayHistoryState = {
        stack: []
    });

    function uncollapseLibPath(path) {
        if (!path) {
            return [];
        }

        const parts = path.split('~').filter((part) => part !== '');
        if (parts.length < 2) {
            return [];
        }

        const lastPart = parts[parts.length - 1];
        const extensionIndex = lastPart.lastIndexOf('.');
        const extension = extensionIndex >= 0 ? lastPart.slice(extensionIndex) : '';
        const fileParts = parts.slice(0, -1);
        const files = [];
        let currentDir = '';

        fileParts.forEach((item) => {
            if (!item) {
                return;
            }
            if (item.startsWith('/')) {
                currentDir = item.slice(0, item.lastIndexOf('/'));
                files.push(item + extension);
                return;
            }

            const resolved = currentDir ? `${currentDir}/${item}` : `/${item}`;
            const slashIndex = resolved.lastIndexOf('/');
            if (slashIndex >= 0) {
                currentDir = resolved.slice(0, slashIndex);
            }
            files.push(`${resolved}${extension}`);
        });

        return files;
    }

    function stylesheetIncludesFile(href, targetFile) {
        if (!href) {
            return false;
        }

        let url;
        try {
            url = new URL(href, window.location.href);
        } catch (_) {
            return false;
        }

        const pathname = url.pathname || '';
        if (pathname === developmentCssHref || pathname.endsWith(`/${targetFile}`)) {
            return true;
        }

        const segments = pathname.split('/');
        const libIndex = segments.findIndex((segment) =>
            segment === 'lib' ||
            segment === 'lib_min' ||
            segment === 'lib_nocache' ||
            segment === 'lib_min_nocache');

        if (libIndex < 0 || libIndex === segments.length - 1) {
            return false;
        }

        let collapsed;
        try {
            collapsed = decodeURIComponent(segments.slice(libIndex + 1).join('/'));
        } catch (_) {
            return false;
        }
        return uncollapseLibPath(collapsed).some((file) => file.replace(/^\//, '') === targetFile);
    }

    function ensureDevelopmentCss() {
        const existingLink = Array.from(document.querySelectorAll('link[rel~="stylesheet"][href]')).find((link) => {
            const href = link.getAttribute('href') || '';
            return stylesheetIncludesFile(href, developmentCssFile);
        });

        if (existingLink) {
            return;
        }

        const link = document.createElement('link');
        link.rel = 'stylesheet';
        link.href = developmentCssHref;
        document.head.appendChild(link);
    }

    function setDebugMessage(message) {
        debugData.replaceChildren();
        const paragraph = document.createElement('p');
        paragraph.className = 'help-block';
        paragraph.textContent = message;
        debugData.appendChild(paragraph);
    }

    function showDebugAlert() {
        debugAlert.style.display = '';
    }

    function hideDebugAlert() {
        debugAlert.style.display = 'none';
    }

    function checkedDebugPoints() {
        return Array.from(document.querySelectorAll('.template-debug-source input:checked'))
            .map((input) => input.value);
    }

    function clearLineHighlights() {
        document.querySelectorAll('.template-compiler-line.highlighted')
            .forEach((line) => line.classList.remove('highlighted'));
    }

    function highlightLine(line) {
        if (!line) {
            return;
        }
        document.querySelectorAll(`.template-compiler-line[data-line="${line}"]`)
            .forEach((lineEl) => lineEl.classList.add('highlighted'));
    }

    function updateBackButton() {
        templateBackButton.disabled = overlayHistoryState.stack.length === 0;
    }

    function createDropdown(label, title) {
        const dropdown = document.createElement('div');
        dropdown.className = 'template-debug-dropdown';
        return initializeDropdown(dropdown, label, title);
    }

    function initializeDropdown(dropdown, label, title) {
        dropdown.replaceChildren();

        const button = document.createElement('button');
        button.type = 'button';
        button.className = 'template-debug-dropdown-toggle';
        button.textContent = label;
        if (title) {
            button.title = title;
            button.setAttribute('aria-label', title);
        }

        const menu = document.createElement('div');
        menu.className = 'template-debug-dropdown-menu';

        dropdown.appendChild(button);
        dropdown.appendChild(menu);
        dropdown._button = button;
        dropdown._menu = menu;
        return dropdown;
    }

    function closeDropdown(dropdown) {
        if (!dropdown) {
            return;
        }
        dropdown.classList.remove('is-open');
    }

    function closeAllDropdowns(exceptDropdown) {
        document.querySelectorAll('.template-debug-dropdown.is-open').forEach((dropdown) => {
            if (dropdown !== exceptDropdown) {
                closeDropdown(dropdown);
            }
        });
    }

    function setDropdownStatus(dropdown, label, className) {
        dropdown._menu.replaceChildren();
        const item = document.createElement('div');
        item.className = className || 'template-debug-dropdown-empty';
        item.textContent = label;
        dropdown._menu.appendChild(item);
    }

    function setDropdownTemplates(dropdown, emptyLabel, templates) {
        dropdown._menu.replaceChildren();
        if (!templates.length) {
            setDropdownStatus(dropdown, emptyLabel, 'template-debug-dropdown-empty');
            return;
        }

        templates.forEach((templateInfo) => {
            const item = document.createElement('button');
            item.type = 'button';
            item.className = 'template-debug-dropdown-item';
            item.textContent = templateInfo.module
                ? `${templateInfo.name} (${templateInfo.module})`
                : templateInfo.name;
            item.addEventListener('click', () => {
                closeDropdown(dropdown);
                navigateToTemplate(templateInfo.template);
            });
            dropdown._menu.appendChild(item);
        });
    }

    function templateCall(topic, payload) {
        return cotonic.broker.call(topic, payload).then((msg) => msg.payload.result || []);
    }

    function loadParentTemplates(dropdown) {
        if (dropdown.dataset.loading === '1') {
            return;
        }

        dropdown.dataset.loading = '1';
        setDropdownStatus(dropdown, '{_ Loading... _}', 'template-debug-dropdown-empty');
        templateCall(
            'bridge/origin/model/development/get/template_trace_parents',
            { template: currentTemplateFile }
        ).then((templates) => {
            setDropdownTemplates(dropdown, '{_ No traced parent templates _}', templates);
        }).catch(() => {
            setDropdownStatus(dropdown, '{_ Could not load templates _}', 'template-debug-dropdown-empty');
        }).finally(() => {
            delete dropdown.dataset.loading;
        });
    }

    function loadChildTemplates(dropdown) {
        if (dropdown.dataset.loading === '1') {
            return;
        }

        const line = Number.parseInt(dropdown.dataset.line || '', 10);
        const column = Number.parseInt(dropdown.dataset.column || '', 10);
        const type = dropdown.dataset.type || '';
        const isPositionMatched = type === 'include';
        const topic = type === 'include'
            ? 'bridge/origin/model/development/get/template_trace_children'
            : 'bridge/origin/model/development/get/template_trace_parents';
        dropdown.dataset.loading = '1';
        setDropdownStatus(dropdown, '{_ Loading... _}', 'template-debug-dropdown-empty');
        templateCall(
            topic,
            {
                template: currentTemplateFile,
                line: isPositionMatched && Number.isInteger(line) ? line : 0,
                column: isPositionMatched && Number.isInteger(column) ? column : 0,
                type: type
            }
        ).then((templates) => {
            setDropdownTemplates(dropdown, '{_ No traced templates on this point _}', templates);
        }).catch(() => {
            setDropdownStatus(dropdown, '{_ Could not load templates _}', 'template-debug-dropdown-empty');
        }).finally(() => {
            delete dropdown.dataset.loading;
        });
    }

    function navigateToTemplate(templatePath, options = {}) {
        const pushHistory = options.pushHistory !== false;
        if (!templatePath || templatePath === currentTemplateFile) {
            return;
        }

        if (pushHistory) {
            const lastTemplate = overlayHistoryState.stack[overlayHistoryState.stack.length - 1];
            if (lastTemplate !== currentTemplateFile) {
                overlayHistoryState.stack.push(currentTemplateFile);
            }
        }
        updateBackButton();

        const overlay = overlayDebug.closest('.modal-overlay');
        const closeButton = overlay ? overlay.querySelector('.modal-overlay-close') : null;
        if (closeButton) {
            closeButton.click();
        }
        window.setTimeout(() => {
            z_event('template_view', { template: templatePath });
        }, 0);
    }

    function navigateBack() {
        while (overlayHistoryState.stack.length > 0) {
            const previousTemplate = overlayHistoryState.stack.pop();
            if (previousTemplate && previousTemplate !== currentTemplateFile) {
                updateBackButton();
                navigateToTemplate(previousTemplate, { pushHistory: false });
                return;
            }
        }
        updateBackButton();
    }

    function bindDropdown(dropdown, loadFn) {
        dropdown._button.addEventListener('click', (event) => {
            event.preventDefault();
            const willOpen = !dropdown.classList.contains('is-open');
            closeAllDropdowns(dropdown);
            if (!willOpen) {
                closeDropdown(dropdown);
                return;
            }
            dropdown.classList.add('is-open');
            loadFn(dropdown);
        });
    }

    function createTemplatePointDropdown(target, options = {}) {
        const dropdown = createDropdown('↗', '{_ Open included template _}');
        dropdown.classList.add('template-debug-line-nav');
        dropdown.dataset.line = options.line || '';
        dropdown.dataset.column = options.column || '';
        dropdown.dataset.type = options.type || '';
        setDropdownStatus(dropdown, '{_ Click to load templates _}', 'template-debug-dropdown-empty');
        bindDropdown(dropdown, loadChildTemplates);
        target.appendChild(dropdown);
        return dropdown;
    }

    function injectTemplateNavigation() {
        initializeDropdown(templateParentsDropdown, '↑', '{_ Included by _}');
        bindDropdown(templateParentsDropdown, loadParentTemplates);
        setDropdownStatus(templateParentsDropdown, '{_ Click to load templates _}', 'template-debug-dropdown-empty');
        templateBackButton.addEventListener('click', navigateBack);
        updateBackButton();

        const navAnchors = document.querySelectorAll('.template-compiler-nav-anchor[data-template-nav-enabled="1"]');
        navAnchors.forEach((anchorEl) => {
            if (anchorEl.querySelector('.template-debug-line-nav')) {
                return;
            }
            createTemplatePointDropdown(anchorEl, {
                line: anchorEl.dataset.line || '',
                column: anchorEl.dataset.column || '',
                type: anchorEl.dataset.templateNav || ''
            });
        });
    }

    document.addEventListener('click', (event) => {
        if (!event.target.closest('.template-debug-dropdown')) {
            closeAllDropdowns(null);
        }
    });

    function termText(text, className) {
        const span = document.createElement('span');
        span.className = className ? `template-debug-term ${className}` : 'template-debug-term';
        span.textContent = text;
        return span;
    }

    function appendInline(target, pieces) {
        pieces.forEach((piece) => {
            if (piece instanceof Node) {
                target.appendChild(piece);
            } else {
                target.appendChild(document.createTextNode(piece));
            }
        });
    }

    function objectKeys(value) {
        return Object.keys(value || {}).filter((key) => key !== '_type');
    }

    function taggedType(value) {
        if (!value || typeof value !== 'object' || Array.isArray(value)) {
            return null;
        }

        if (typeof value._type === 'string') {
            return value._type.startsWith('_') ? value._type : `_${value._type}`;
        }

        return null;
    }

    function taggedValue(value, ...keys) {
        for (const key of keys) {
            if (value && Object.prototype.hasOwnProperty.call(value, key)) {
                return value[key];
            }
        }
        return undefined;
    }

    function isPrintableString(value) {
        return typeof value === 'string' && /^[\u0020-\u007e]*$/.test(value);
    }

    function previewTerm(value, depth = 0) {
        if (depth > 1) {
            return '...';
        }

        const type = taggedType(value);
        if (type) {
            switch (type) {
                case '_tuple': {
                    const items = taggedValue(value, 'value', 'items', 'elements', '_list');
                    const preview = Array.isArray(items)
                        ? items.slice(0, 3).map((item) => previewTerm(item, depth + 1)).join(', ')
                        : '';
                    const suffix = Array.isArray(items) && items.length > 3 ? ', ...' : '';
                    return `{${preview}${suffix}}`;
                }
                case '_atom':
                    return String(taggedValue(value, 'value', 'atom', 'name') ?? 'undefined');
                case '_binary': {
                    const binaryValue = taggedValue(value, 'value', 'data');
                    if (typeof binaryValue === 'string') {
                        return isPrintableString(binaryValue)
                            ? `<<"${binaryValue}">>`
                            : `<<${binaryValue.length} bytes>>`;
                    }
                    return '<<...>>';
                }
                case '_record': {
                    const name = taggedValue(value, 'name', 'record') ?? 'record';
                    return `#${name}{...}`;
                }
                case '_map':
                    return '#{...}';
                case '_list':
                    return '[...]';
                case '_pid':
                case '_port':
                case '_reference':
                case '_fun':
                    return String(taggedValue(value, 'value', 'id', 'name') ?? type);
                default:
                    return `${type}(...)`;
            }
        }

        if (Array.isArray(value)) {
            const preview = value.slice(0, 3).map((item) => previewTerm(item, depth + 1)).join(', ');
            return `[${preview}${value.length > 3 ? ', ...' : ''}]`;
        }

        if (value === null) {
            return 'null';
        }

        switch (typeof value) {
            case 'string':
                return `"${value}"`;
            case 'number':
            case 'boolean':
                return String(value);
            case 'object': {
                const keys = Object.keys(value);
                const preview = keys.slice(0, 2).map((key) => `${key} => ${previewTerm(value[key], depth + 1)}`).join(', ');
                return `#{${preview}${keys.length > 2 ? ', ...' : ''}}`;
            }
            default:
                return String(value);
        }
    }

    function renderPrimitive(value) {
        if (value === null) {
            return termText('null', 'is-null');
        }
        switch (typeof value) {
            case 'string':
                return termText(JSON.stringify(value), 'is-string');
            case 'number':
                return termText(String(value), 'is-number');
            case 'boolean':
                return termText(String(value), 'is-atom');
            default:
                return termText(String(value), 'is-special');
        }
    }

    function renderRows(rows, separator) {
        const container = document.createElement('div');
        container.className = 'template-debug-term-children';

        rows.forEach((row, index) => {
            const rowEl = document.createElement('div');
            rowEl.className = 'template-debug-term-row';

            if (row.label !== null && row.label !== undefined) {
                const label = document.createElement('span');
                label.className = 'template-debug-term-label';
                label.textContent = row.label;
                rowEl.appendChild(label);
            }

            const valueEl = document.createElement('div');
            valueEl.className = 'template-debug-term-value';
            valueEl.appendChild(row.value);
            rowEl.appendChild(valueEl);

            if (separator && index < rows.length - 1) {
                valueEl.appendChild(termText(separator, 'is-punctuation'));
            }

            container.appendChild(rowEl);
        });

        return container;
    }

    function renderCompound(summaryParts, rows, closingText, isOpen = false, separator = ',') {
        const details = document.createElement('details');
        details.className = 'template-debug-value';
        details.open = isOpen;

        const summary = document.createElement('summary');
        appendInline(summary, summaryParts);
        details.appendChild(summary);

        if (rows.length) {
            details.appendChild(renderRows(rows, separator));
        }

        const closing = document.createElement('div');
        closing.className = 'template-debug-term-children';
        closing.appendChild(termText(closingText, 'is-punctuation'));
        details.appendChild(closing);

        return details;
    }

    function renderTaggedTerm(value, depth) {
        const type = taggedType(value);
        switch (type) {
            case '_tuple': {
                const items = taggedValue(value, 'value', 'items', 'elements', '_list');
                const rows = Array.isArray(items)
                    ? items.map((item, index) => ({ label: `${index + 1}:`, value: renderTerm(item, depth + 1) }))
                    : [];
                return renderCompound(
                    [ termText(previewTerm(value), 'is-special') ],
                    rows,
                    '}',
                    depth < 1
                );
            }
            case '_list': {
                const items = taggedValue(value, 'value', 'items', 'elements');
                const rows = Array.isArray(items)
                    ? items.map((item, index) => ({ label: `${index + 1}:`, value: renderTerm(item, depth + 1) }))
                    : [];
                return renderCompound(
                    [ termText(previewTerm(value), 'is-special') ],
                    rows,
                    ']',
                    false
                );
            }
            case '_map': {
                const mapValue = taggedValue(value, 'value', 'map', 'entries');
                if (Array.isArray(mapValue)) {
                    const rows = mapValue.map((entry) => {
                        const key = Array.isArray(entry) ? entry[0] : '?';
                        const entryValue = Array.isArray(entry) ? entry[1] : entry;
                        return { label: `${previewTerm(key)} =>`, value: renderTerm(entryValue, depth + 1) };
                    });
                    return renderCompound(
                        [ termText(previewTerm(value), 'is-special') ],
                        rows,
                        '}',
                        false
                    );
                }
                return renderTerm(mapValue || {}, depth);
            }
            case '_record': {
                const recordName = taggedValue(value, 'name', 'record') ?? 'record';
                const fields = taggedValue(value, 'value', 'fields');
                const rows = [];

                if (fields && typeof fields === 'object' && !Array.isArray(fields)) {
                    Object.entries(fields).forEach(([key, fieldValue]) => {
                        rows.push({ label: `${key} =`, value: renderTerm(fieldValue, depth + 1) });
                    });
                } else {
                    objectKeys(value).forEach((key) => {
                        if (key !== 'name' && key !== 'record' && key !== 'value' && key !== 'fields') {
                            rows.push({ label: `${key} =`, value: renderTerm(value[key], depth + 1) });
                        }
                    });
                }

                return renderCompound(
                    [ termText(previewTerm(value), 'is-tag') ],
                    rows,
                    '}',
                    false
                );
            }
            case '_atom':
                return termText(String(taggedValue(value, 'value', 'atom', 'name') ?? 'undefined'), 'is-atom');
            case '_binary': {
                const binaryValue = taggedValue(value, 'value', 'data');
                if (typeof binaryValue === 'string') {
                    return termText(
                        isPrintableString(binaryValue)
                            ? `<<"${binaryValue}">>`
                            : `<<${binaryValue.length} bytes>>`,
                        'is-binary'
                    );
                }
                return termText('<<...>>', 'is-binary');
            }
            case '_pid':
            case '_port':
            case '_reference':
            case '_fun':
                return termText(String(taggedValue(value, 'value', 'id', 'name') ?? type), 'is-special');
            default: {
                const rows = objectKeys(value).map((key) => ({
                    label: `${key}:`,
                    value: renderTerm(value[key], depth + 1)
                }));
                return renderCompound(
                    [ termText(previewTerm(value), 'is-tag') ],
                    rows,
                    ')',
                    false
                );
            }
        }
    }

    function renderObject(value, depth) {
        const rows = Object.keys(value).map((key) => ({
            label: `${key} =>`,
            value: renderTerm(value[key], depth + 1)
        }));

        return renderCompound(
            [ termText(previewTerm(value), 'is-special') ],
            rows,
            '}',
            depth < 1
        );
    }

    function renderArray(value, depth) {
        const rows = value.map((item, index) => ({
            label: `${index + 1}:`,
            value: renderTerm(item, depth + 1)
        }));

        return renderCompound(
            [ termText(previewTerm(value), 'is-special') ],
            rows,
            ']',
            depth < 1
        );
    }

    function renderTerm(value, depth = 0) {
        const type = taggedType(value);
        if (type) {
            return renderTaggedTerm(value, depth);
        }
        if (Array.isArray(value)) {
            return renderArray(value, depth);
        }
        if (value && typeof value === 'object') {
            return renderObject(value, depth);
        }
        return renderPrimitive(value);
    }

    if (typeof window.restartTemplateTrace == "function") {
        debugRestart.style.display = '';
        debugRestartButton.addEventListener('click', window.restartTemplateTrace);
    }

    function debug_start() {
        const is_all = overlayDebug.querySelector('.template-debug-data input[value="all"]').checked;
        const enabled = checkedDebugPoints();

        showDebugAlert();
        if (enabled.length === 0) {
            setDebugMessage('{_ Check debug points in the template source to see debug data here. _}');
        } else {
            setDebugMessage('{_ Waiting for debug data... _}');
        }

        is_debug_trace_enabled = enabled.length > 0;

        z_event("template_debug_enable", {
            enabled: enabled,
            is_all: is_all
        });
    }

    document.querySelectorAll('.template-debug input')
        .forEach((input) => input.addEventListener('input', debug_start));
    debugStartButton.addEventListener('click', debug_start);

    document.querySelectorAll('.modal-overlay-close')
        .forEach((button) => button.addEventListener('click', () => {
            if (is_debug_trace_enabled) {
                z_event("template_debug_enable", {
                    enabled: [],
                    is_all: false,
                    is_silent: true
                });
            }
        }));

    ['mouseover', 'mouseout'].forEach((eventName) => {
        debugData.addEventListener(eventName, (e) => {
            const details = e.target.closest('details[data-line]');
            if (!details || !debugData.contains(details)) {
                return;
            }

            const related = e.relatedTarget;
            if (related instanceof Node && details.contains(related)) {
                return;
            }

            clearLineHighlights();
            if (eventName === 'mouseover') {
                highlightLine(details.dataset.line);
            }
        });
    });

    ensureDevelopmentCss();
    injectTemplateNavigation();

    cotonic.broker.subscribe(
        "bridge/origin/model/development/event/template/debug",
        (msg) => {
            const payload = msg.payload;
            switch (payload.event) {
                case "data":
                    if (payload.filename == '{{ template_file|escapejs }}') {
                        const id = `template-data-${payload.line}-${payload.column}`;
                        let root = document.getElementById(id);

                        if (!root) {
                            root = document.createElement('details');
                            root.id = id;
                            root.dataset.line = payload.line;
                            const summary = document.createElement('summary');
                            summary.textContent = `Line ${payload.line}:${payload.column}`;
                            root.appendChild(summary);
                            debugData.appendChild(root);
                            const helpBlock = debugData.querySelector(':scope > .help-block');
                            if (helpBlock) {
                                helpBlock.remove();
                            }
                        }

                        const table = document.createElement('table');
                        table.className = 'table table-striped';
                        const tbody = document.createElement('tbody');
                        for (const [key, value] of Object.entries(payload.data)) {
                            const tr = document.createElement('tr');
                            const th = document.createElement('th');
                            const td = document.createElement('td');

                            th.textContent = key;
                            td.appendChild(renderTerm(value));
                            tr.append(th, td);
                            tbody.appendChild(tr);
                        }
                        table.appendChild(tbody);
                        root.appendChild(table);
                    }
                    break;
                case "stop":
                    showDebugAlert();
                    break;
                case "start":
                    hideDebugAlert();
                    break;
                default:
                    break;
            }
        });
{% endjavascript %}
