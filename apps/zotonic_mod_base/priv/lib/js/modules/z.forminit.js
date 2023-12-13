/* forminit js
----------------------------------------------------------

@package:   Zotonic 2023
@author:    Marc Worrell

Copyright 2023 Marc Worrell

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

$.widget("ui.forminit",
{
    _init: function()
    {
        // Build a lookup list of values in the query string.
        // Keys with multiple values are added as arrays.
        function parseQs ( qs ) {
            let q = {};
            let ps = [];

            const searchParams = new URLSearchParams(qs);
            searchParams.forEach(function(value, key) {
                if (typeof q[key] === 'undefined') {
                    q[key] = [value];
                } else {
                    q[key].push(value);
                }
            });
            return q;
        }

        let controls = this.element[0].elements;
        let args = parseQs(window.location.search);

        // Loop over all controls, set the input and selects
        // to their value in the parsed args. For input texts,
        // remove any value found in the args, so that we can
        // initialize multiple elements with the same name.

        // HTMLFormControlsCollection
        for (let i = 0; i < controls.length; i++) {
            const control = controls[i];
            const name = control.name;

            if (typeof args[name] === 'object' && args[name].length > 0) {
                if (control.nodeName === "INPUT") {
                    switch (controls[i].type) {
                        case 'file':
                        case 'submit':
                        case 'reset':
                        case 'button':
                            break;
                        case 'radio':
                        case 'checkbox':
                            if (args[name].includes(control.value)) {
                                control.checked = true;
                            } else {
                                control.checked = false;
                            }
                            break;
                        default:
                            control.value = args[name].shift() ?? "";
                            break;
                    }
                } else if (control.nodeName === "TEXTAREA") {
                    control.value = args[name].shift() ?? "";
                } else if (control.nodeName === "SELECT") {
                    // Select correct option - TODO: handle multiple select
                    for (let k = 0; k < control.options.length; k++) {
                        if (args[name].includes(control.options[k].value)) {
                            controls[i].selectedIndex = k;
                            break;
                        }
                    }
                }
            } else {
                if (controls[i].nodeName === "INPUT") {
                    switch (control.type) {
                        case 'file':
                        case 'submit':
                        case 'reset':
                        case 'button':
                            break;
                        case 'radio':
                        case 'checkbox':
                            control.checked = false;
                            break;
                        default:
                            control.value = "";
                            break;
                    }
                } else if (control.nodeName === "TEXTAREA") {
                    control.value = "";
                } else if (control.nodeName === "SELECT") {
                    control.selectedIndex = 0;
                }
            }
        }
    }
});

$.ui.forminit.defaults = {
};
