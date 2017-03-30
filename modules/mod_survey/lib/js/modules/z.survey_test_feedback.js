/* z.survey_test_feedback js
----------------------------------------------------------

Copyright 2017 Marc Worrell

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

$.widget("ui.survey_test_feedback", 
{
    _init: function() 
    {
        var self = this;
        var obj  = this.element;

        $(obj).on(
            'click',
            'input',
            function(ev) {
                $(this)
                    .closest('label')
                    .removeClass("survey-test-feedback")
                    .removeClass("survey-q-ok")
                    .removeClass("survey-q-not-ok")
                    .find(".survey-test-feedback-icon")
                    .remove();

                if ($(this).is(':checked')) {
                    var cls = 'survey-q-ok';
                    var icn = 'fa fa-check';

                    if ($(this).attr('data-is-correct') != 'true') {
                        cls = 'survey-q-not-ok';
                        icn = 'fa fa-remove';
                    }
                    $(this)
                        .closest('label')
                        .addClass("survey-test-feedback")
                        .addClass(cls)
                        .append("<span class='survey-test-feedback-icon'><span class='"+icn+"'></span></span>");
                }
            });
        $(obj).on(
            'click',
            'select',
            function(ev) {
                var val = $(this).val();
                $(this)
                    .closest('div')
                    .removeClass("survey-test-feedback")
                    .removeClass("survey-q-ok")
                    .removeClass("survey-q-not-ok")
                    .find(".survey-test-feedback-icon")
                    .remove();

                if (val !== '') {
                    var cls = 'survey-q-ok';
                    var icn = 'fa fa-check';

                    if ($(this).attr('data-is-correct') != val) {
                        cls = 'survey-q-not-ok';
                        icn = 'fa fa-remove';
                    }
                    $(this)
                        .closest('div')
                        .addClass("survey-test-feedback")
                        .addClass(cls)
                        .append("<span class='survey-test-feedback-icon'><span class='"+icn+"'></span></span>");
                }
            });    }
});

$.fn.tooltip.destroy = function()
{
    $('.tooltip').tooltip('destroy');
};

$.ui.tooltip.defaults = {
    offsetY:    0,
    offsetX:    0,
    inevent:    'mouseover',
    outevent:   'mouseout',
    width:      'auto',
    maxwidth:   '330px'
};
