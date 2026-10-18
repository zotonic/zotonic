/* Copyright 2026 Marc Worrell. Licensed under the Apache License, Version 2.0. */
/* Keep asynchronous row arrivals newest-first, and deduplicated without replacing existing rows. */
window.z_import_feed_log_rows = function() {
    const body = document.getElementById("feed-log-rows");
    if (!body) return;
    const seen = new Set();
    const rows = Array.from(body.querySelectorAll("tr[data-log-id]"))
        .filter(row => {
            if (seen.has(row.dataset.logId)) {
                row.remove();
                return false;
            }
            seen.add(row.dataset.logId);
            return true;
        })
        .sort((a, b) => Number(b.dataset.logId) - Number(a.dataset.logId));
    if (rows.length) document.getElementById("feed-log-empty")?.remove();
    rows.forEach((row, index) => {
        if (body.children[index] !== row) body.insertBefore(row, body.children[index] || null);
    });
};
