/* autofocus js
----------------------------------------------------------

@package: Zotonic 2026

Copyright 2026 Zotonic Foundation

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
    $.widget("ui.autofocus", {
        _init: function() {
            const focus = () => this.element.focus();
            const $modal = this.element.closest(".modal");

            if ($modal.length) {
                $modal.one("shown.bs.modal", focus);
            }
            setTimeout(focus, 0);
        }
    });
})(jQuery);
