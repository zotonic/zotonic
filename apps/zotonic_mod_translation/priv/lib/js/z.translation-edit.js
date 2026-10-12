/* Copyright 2026 Zotonic. Apache-2.0. */
(function () {
    "use strict";
    if (window.z_translation_edit_installed) return;
    window.z_translation_edit_installed = true;
    document.addEventListener('keydown', event => {
        if (event.target.matches('.translation-edit-text') && ['Enter', ' '].includes(event.key)) {
            event.preventDefault();
            event.target.click();
        }
    });
    document.addEventListener('click', event => {
        // Preview links belong to the editable cell, not navigation away from the overview.
        if (event.target.closest('.translation-edit-text a')) event.preventDefault();
    });
    $(document).on('hide.bs.modal', '.modal', function (event) {
        if (event.target === this && this.querySelector('[data-translation-edit-form]')) {
            window.z_editor?.remove($(this));
        }
    });
})();
