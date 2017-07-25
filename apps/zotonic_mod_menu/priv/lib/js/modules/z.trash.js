/* z.trash.js
----------------------------------------------------------

@author Marc Worrell <marc@worrell.nl>

Copyright 2011 Marc Worrell

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

$.widget("ui.trash", {
    _init: function() {
        var self = this;
        $(this.element)
            .addClass("ui-trash")
            .sortable({
                dropOnEmpty: true,
                cursor: 'pointer',
                tolerance: 'pointer',
                accept: self.options.accept,
                update: function(event, ui) { $('#'+ui.item.attr('id')).remove(); },
                over: function() { $(self.element).addClass('ui-trash-over'); },
                out: function() { $(self.element).removeClass('ui-trash-over'); }
        });
    }
});

$.ui.trash.defaults = {
    accept: "li,ul"
};
