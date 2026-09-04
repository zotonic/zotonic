/* autoheight js
----------------------------------------------------------

@package: Zotonic 2026

Copyright 2026 Marc Worrell

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

---------------------------------------------------------- */

(function($) {
    "use strict";

    /**
     * Resize a textarea to its content, up to the configured maximum height.
     * The do_autoheight class can be placed on a textarea or a container.
     */
    const cssPixels = value => Number.parseFloat(value) || 0;

    const resizeTextarea = (textarea, maxHeight) => {
        if (textarea.getClientRects().length === 0) {
            return;
        }

        const scrollContainer = textarea.closest(".scroll");
        const scrollTop = scrollContainer?.scrollTop;
        const style = window.getComputedStyle(textarea);
        const padding = cssPixels(style.paddingTop) + cssPixels(style.paddingBottom);
        const border = cssPixels(style.borderTopWidth) + cssPixels(style.borderBottomWidth);

        textarea.style.height = "auto";
        textarea.style.overflowY = "hidden";

        const naturalHeight = style.boxSizing === "border-box"
            ? textarea.scrollHeight + border
            : textarea.scrollHeight - padding;
        const height = Number.isFinite(maxHeight)
            ? Math.min(naturalHeight, maxHeight)
            : naturalHeight;

        textarea.style.height = `${Math.ceil(height)}px`;
        textarea.style.overflowY = naturalHeight > height ? "auto" : "hidden";

        if (scrollContainer) {
            scrollContainer.scrollTop = scrollTop;
        }
    };

    $.widget("ui.autoheight", {
        _init: function() {
            this._autoheightDestroy?.();

            const root = this.element[0];
            const textareas = root.matches("textarea")
                ? [root]
                : Array.from(root.querySelectorAll("textarea"));

            if (textareas.length === 0) {
                return;
            }

            const parsedMaxHeight = Number.parseFloat(this.options.maxHeight);
            const maxHeight = parsedMaxHeight > 0 ? parsedMaxHeight : Infinity;
            const form = root.matches("form") ? root : root.closest("form");
            let animationFrame;

            const resizeAll = () => {
                animationFrame = undefined;
                textareas.forEach(textarea => resizeTextarea(textarea, maxHeight));
            };
            const scheduleResize = () => {
                if (animationFrame === undefined) {
                    animationFrame = window.requestAnimationFrame(resizeAll);
                }
            };

            textareas.forEach(textarea => {
                textarea.addEventListener("input", scheduleResize);
                textarea.addEventListener("change", scheduleResize);
            });
            form?.addEventListener("reset", scheduleResize);
            window.addEventListener("resize", scheduleResize);
            $(document).on(
                "shown.bs.collapse shown.bs.modal shown.bs.tab",
                scheduleResize
            );

            this._autoheightDestroy = () => {
                if (animationFrame !== undefined) {
                    window.cancelAnimationFrame(animationFrame);
                }
                textareas.forEach(textarea => {
                    textarea.removeEventListener("input", scheduleResize);
                    textarea.removeEventListener("change", scheduleResize);
                });
                form?.removeEventListener("reset", scheduleResize);
                window.removeEventListener("resize", scheduleResize);
                $(document).off(
                    "shown.bs.collapse shown.bs.modal shown.bs.tab",
                    scheduleResize
                );
            };

            scheduleResize();
        },

        _destroy: function() {
            this._autoheightDestroy?.();
        }
    });

    $.ui.autoheight.defaults = {
        maxHeight: 500
    };
})(jQuery);
