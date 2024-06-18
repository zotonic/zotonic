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

/**
 * On form reset:
 * - Prevent specified 'input' and 'select' elements in the form from being reset by a 'reset' event/input.
 *   Elements are ignored by reset if they have the attribute `data-skip-reset`
 * - Push an 'input' event to re-submit the form itself
 */
const configureFormReset = (formElement) => {
    $(formElement).on('reset', (e) => {
        /**
         * Set element's defaultValue to selected value, before reset sets value to defaultValue.
         */
        $( formElement ).find('[data-skip-reset]').each(( index, skippedElement ) => {

            //Select
            if($(skippedElement).is('select')) {
                $( skippedElement )
                    .find('option')
                    .each(( optionIndex, optionEl ) => {
                        optionEl.defaultSelected = optionEl.selected;
                });
                return;
            }
            //Checkbox
            skippedElement.defaultChecked = skippedElement.checked;
            //Other inputs
            skippedElement.defaultValue = skippedElement.value;
        });

        //Force submit
        setTimeout(function () {
            e.target.dispatchEvent(new Event('input', {bubbles: true}));
        }, 0);
    })
}

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
                    // NOTE: If number of options change you cannot use
                    // method="patch" in live tag as _init will not be
                    // called subsequently
                    for (let k = 0; k < control.options.length; k++) {
                        if( control.hasAttribute("multiple")) {
                            if (args[name].includes(control.options[k].value)) {
                                controls[i].options[k].selected = true;
                            } else {
                                controls[i].options[k].selected = false;
                            }
                        } else {
                            if (args[name].includes(control.options[k].value)) {
                                controls[i].selectedIndex = k;
                                break;
                            }
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
        // TODO: make this optional or a separate widget?
        configureFormReset(this.element);
    }
});

$.ui.forminit.defaults = {
};
