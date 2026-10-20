/* Shared saved-content preview for admin and admin_frontend. */
(function() {
    "use strict";

    cotonic.ready.then(function() {
        const panel = document.getElementById("admin-edit-preview");
        if (!panel || panel.dataset.initialized) return;
        panel.dataset.initialized = "true";

        const broker = cotonic.broker;
        const frame = panel.querySelector("iframe");
        const languageTag = panel.querySelector("[data-admin-preview-language]");
        const unavailable = panel.querySelector("[data-admin-preview-unavailable]");
        const link = panel.querySelector("[data-admin-preview-link]");
        const options = { wid: "admin-edit-preview" };
        let context = {};
        let topics = [];
        let timer;
        let lastUrl;
        let open = false;
        let interacted = false;

        function updateDirty() {
            panel.querySelector("[data-admin-preview-dirty]").hidden =
                !document.querySelector('#rscform[data-formdirty]');
        }

        new MutationObserver(updateDirty).observe(document.body, {
            subtree: true, attributes: true, attributeFilter: ["data-formdirty"]
        });

        function updateLanguage() {
            const tab = document.querySelector(
                '#translation-tabs .language-tabs li.active[lang], #rscform .language-tabs li.active[lang]'
            );
            const language = tab?.getAttribute("lang") || context.language || panel.dataset.previewLanguage;
            languageTag.textContent = (language || "").toUpperCase();
            if (tab) {
                languageTag.title = tab.textContent.trim();
                // Frontend forms can initially select a different language than the UI.
                z_postback_data_set("z_edit_language", language);
            }
        }

        function refresh() {
            clearTimeout(timer);
            updateLanguage();
            updateDirty();
            if (open && context.id) {
                // Coalesce resource, medium and edge events from a single edit.
                timer = setTimeout(() => z_event("admin-preview-resolve"), 250);
            }
        }

        function subscribe() {
            if (topics.length) broker.unsubscribe(topics, options);
            topics = open ? [...new Set([context.id, context.tree_id])]
                .filter(id => Number.isInteger(id) && id > 0)
                .flatMap(id => [
                    `bridge/origin/model/rsc/event/${id}/+`,
                    `bridge/origin/model/media/event/${id}/+`,
                    `bridge/origin/model/edge/event/${id}/+/+`
                ]) : [];
            if (topics.length) broker.subscribe(topics, refresh, options);
        }

        function clearFrame() {
            frame.onload = null;
            frame.contentWindow.location.replace("about:blank");
        }

        function setOpen(value) {
            open = value;
            panel.hidden = !open;
            document.body.classList.toggle("admin-preview-open", open);
            document.querySelectorAll('[data-admin-preview="toggle"]').forEach(button => {
                button.setAttribute("aria-expanded", String(open));
                button.querySelector("[data-admin-preview-label]").textContent =
                    open ? button.dataset.panelCloseLabel : button.dataset.panelOpenLabel;
            });
            subscribe();
            if (open) {
                refresh();
            } else {
                clearTimeout(timer);
                clearFrame();
                lastUrl = undefined;
            }
        }

        broker.subscribe("admin/preview/context", function(msg) {
            context = msg.payload;
            clearTimeout(timer);
            clearFrame();
            link.removeAttribute("href");
            lastUrl = undefined;
            subscribe();
            refresh();
        }, options);

        broker.subscribe("admin/preview/location", function(msg) {
            if (!open || msg.payload.id !== context.id) return;
            unavailable.hidden = true;
            let url;
            try {
                url = msg.payload.url && new URL(msg.payload.url, window.location.href);
            } catch (_) { /* Missing or invalid page URL. */ }
            if (!url || !["http:", "https:"].includes(url.protocol)) {
                clearFrame();
                link.removeAttribute("href");
                unavailable.hidden = false;
                return;
            }
            link.href = url.href;
            // The iframe uses the existing session and same-origin frame policy.
            if (url.origin !== window.location.origin) {
                unavailable.hidden = false;
                clearFrame();
                return;
            }
            let scroll;
            try {
                if (lastUrl === url.href) {
                    scroll = [frame.contentWindow.scrollX, frame.contentWindow.scrollY];
                }
            } catch (_) { /* The visitor may have followed an external link. */ }
            frame.onload = function() {
                try {
                    // Both editors include this panel, including site overrides. Navigate
                    // from the parent so the iframe can keep blocking top navigation.
                    if (frame.contentDocument?.getElementById("admin-edit-preview")) {
                        window.top.location.assign(frame.contentWindow.location.href);
                        return;
                    }
                    if (scroll) frame.contentWindow.scrollTo(...scroll);
                } catch (_) { /* Cross-origin navigation is not an embedded editor. */ }
            };
            lastUrl = url.href;
            // Also suppress persistence when Fetch Metadata headers are absent.
            // Keep the toolbar's normal "Open in new tab" URL unchanged.
            url.searchParams.set("z_language_persist", "false");
            // Iframe navigations share the editor's session history. Replace
            // automatic loads so Cancel/Back does not step through preview URLs.
            frame.contentWindow.location.replace(url.href);
        }, options);

        document.addEventListener("click", function(event) {
            const button = event.target.closest("[data-admin-preview]");
            if (!button) return;
            interacted = true;
            switch (button.dataset.adminPreview) {
                case "toggle":
                    setOpen(!open);
                    break;
                case "close": setOpen(false); break;
                case "refresh": refresh(); break;
            }
            broker.publish("model/sessionStorage/post/admin-preview-open", open);
        });

        const resizer = panel.querySelector("[data-admin-preview-resizer]");
        let width = 40;
        let resizePointer;
        let resizeStart;
        let widthInteracted = false;

        function setWidth(value) {
            width = Math.max(30, Math.min(60, Number(value) || 40));
            document.body.style.setProperty("--admin-preview-width", `${width}vw`);
            resizer.setAttribute("aria-valuenow", Math.round(width));
        }

        function saveWidth() {
            broker.publish("model/sessionStorage/post/admin-preview-width", width);
        }

        resizer.addEventListener("pointerdown", function(event) {
            if (event.button !== 0 || resizePointer !== undefined) return;
            event.preventDefault();
            widthInteracted = true;
            resizePointer = event.pointerId;
            resizeStart = { x: event.clientX, width };
            resizer.focus();
            resizer.setPointerCapture(resizePointer);
            document.body.classList.add("admin-preview-resizing");
        });
        resizer.addEventListener("pointermove", function(event) {
            if (event.pointerId !== resizePointer) return;
            setWidth(resizeStart.width + (resizeStart.x - event.clientX) / window.innerWidth * 100);
        });
        function endResize(event) {
            if (event.pointerId !== resizePointer) return;
            resizePointer = undefined;
            document.body.classList.remove("admin-preview-resizing");
            if (resizer.hasPointerCapture(event.pointerId)) {
                resizer.releasePointerCapture(event.pointerId);
            }
            saveWidth();
        }
        resizer.addEventListener("pointerup", endResize);
        resizer.addEventListener("pointercancel", endResize);
        resizer.addEventListener("lostpointercapture", endResize);
        resizer.addEventListener("keydown", function(event) {
            const values = { ArrowLeft: width + 2, ArrowRight: width - 2, Home: 30, End: 60 };
            if (!(event.key in values)) return;
            event.preventDefault();
            widthInteracted = true;
            setWidth(values[event.key]);
            saveWidth();
        });

        // Language tabs update the postback language before this deferred resolve.
        $(document).on("shown.bs.tab", ".language-tabs a", refresh);
        broker.call("model/sessionStorage/get/admin-preview-open").then(function(msg) {
            if (!interacted && msg.payload === true) setOpen(true);
        });
        broker.call("model/sessionStorage/get/admin-preview-width").then(function(msg) {
            if (!widthInteracted) setWidth(msg.payload);
        });
    });
})();
