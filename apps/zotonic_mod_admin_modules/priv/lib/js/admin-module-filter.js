/* Copyright 2026 Marc Worrell. Licensed under the Apache License, Version 2.0. */

function z_admin_module_filter() {
    const controls = document.getElementById("module-filters");
    const inputs = Array.from(controls.querySelectorAll("input, select"));
    const [search, activation, runtime, topic] = inputs;
    // Documentation kinds and audiences do not describe module functionality.
    const excludedTopics = new Set([
        "reference", "module", "content_editor", "operator", "integrator",
        "site_administrator", "frontend_developer", "backend_developer"
    ]);
    const normalize = text => text.normalize("NFKC").toLowerCase().replaceAll("_", " ");
    const rows = Array.from(document.querySelectorAll("#module-list [data-module-row]"), element => ({
        element,
        active: element.dataset.active === "true",
        status: element.dataset.status,
        text: normalize(element.dataset.module + " " + Array.from(element.cells)
            .slice(1, 4).map(cell => cell.textContent).join(" ")),
        topics: Array.from(element.querySelectorAll("[data-topic]"), keyword => ({
            name: keyword.dataset.topic,
            label: keyword.textContent
        }))
    }));
    const topics = new Map();
    rows.forEach(row => row.topics.forEach(({name, label}) => {
        if (!excludedTopics.has(name)) {
            const existing = topics.get(name);
            topics.set(name, {label, count: (existing?.count || 0) + 1});
        }
    }));
    Array.from(topics).sort((a, b) => a[1].label.localeCompare(b[1].label))
        .forEach(([name, {label, count}]) => topic.add(new Option(`${label} (${count})`, name)));

    const apply = () => {
        const words = normalize(search.value).trim().split(/\s+/u).filter(Boolean);
        const visible = rows.filter(row => {
            const matches = words.every(word => row.text.includes(word))
                && (!activation.value || row.active === (activation.value === "activated"))
                && (!runtime.value || (runtime.value === "running"
                    ? row.status === "running"
                    : row.active && row.status !== "running"))
                && (!topic.value || row.topics.some(keyword => keyword.name === topic.value));
            row.element.classList.toggle("hidden", !matches);
            return matches;
        });
        document.getElementById("module-filter-count").textContent = `${visible.length} / ${rows.length}`;
        document.getElementById("module-filter-empty").hidden = visible.length > 0 || rows.length === 0;
    };
    const restore = () => {
        const params = new URL(window.location.href).searchParams;
        inputs.forEach(input => {
            input.value = params.get(input.name) || "";
            // Unknown or obsolete select values fall back to All.
            if (input.selectedIndex === -1) input.value = "";
        });
        apply();
    };
    const update = () => {
        apply();
        const url = new URL(window.location.href);
        inputs.forEach(input => {
            if (input.value) url.searchParams.set(input.name, input.value);
            else url.searchParams.delete(input.name);
        });
        window.history.replaceState(window.history.state, "", url);
    };
    search.addEventListener("input", update);
    inputs.slice(1).forEach(input => input.addEventListener("change", update));
    document.getElementById("module-filters-clear").addEventListener("click", () => {
        inputs.forEach(input => { input.value = ""; });
        update();
        search.focus();
    });
    window.addEventListener("popstate", restore);
    restore();
    controls.hidden = false;
}
