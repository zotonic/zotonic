/* z.formdirty.js
----------------------------------------------------------

@author Marc Worrell <marc@worrell.nl>

Copyright 2019 Marc Worrell

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

$.widget("ui.formdirty", {
    _init: function() {
        var html = $(this.element).html;
        var smileys = this.options.smileys;

        this.element.each(function(){
            var self = $(this);
            $(this).on('change z:editorChange', function() {
                self.attr('data-formdirty', 'true');
            });
            $(this).on('z:formSubmit', function() {
                self.removeAttr('data-formdirty');
            });
        });
    }
});

$.ui.formdirty.defaults = {
};

