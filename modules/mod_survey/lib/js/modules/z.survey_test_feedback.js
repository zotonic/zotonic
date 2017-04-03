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
    _init: function() {
        var self = this;
        var obj  = this.element;

        $(obj).on(
            'click',
            'input',
            function(ev) {
                var $wrap;
                if ($(this).attr('type') == 'radio') {
                    $wrap = $(this).closest('div').find('label');
                } else {
                    $wrap = $(this).closest('label');
                }
                self.reset_feedback_icons($wrap);
                if ($(this).is(':checked')) {
                    var $icon_wrap = $(this).closest('label');
                    self.show_feedback($(this), obj, $icon_wrap, 'true');
                }
            });
        $(obj).on(
            'click',
            'select',
            function(ev) {
                var val = $(this).val();
                var $wrap = $(this).closest('div');
                self.reset_feedback_icons($wrap);

                if (val !== '') {
                    var cls = 'survey-q-ok';
                    var icn = 'fa fa-check';
                    self.show_feedback($(this), obj, $wrap, val);
                }
            });
    },

    reset_feedback_icons: function ($wrap) {
        $wrap
            .removeClass("survey-test-feedback")
            .removeClass("survey-q-ok")
            .removeClass("survey-q-not-ok")
            .find(".survey-test-feedback-icon")
            .remove();
    },

    show_feedback: function ($elt, obj, $icon_wrap, value) {
        var cls = 'survey-q-ok';
        var icn = 'fa fa-check';

        if ($elt.attr('data-is-correct') != value) {
            cls = 'survey-q-not-ok';
            icn = 'fa fa-remove';
            $(obj)
                .removeClass('survey-test-correct')
                .addClass('survey-test-wrong');
        } else {
            $(obj)
                .addClass('survey-test-correct')
                .removeClass('survey-test-wrong');
        }
        $icon_wrap
            .addClass("survey-test-feedback")
            .addClass(cls)
            .append("<span class='survey-test-feedback-icon'><span class='"+icn+"'></span></span>");
    }
});

$.ui.survey_test_feedback.defaults = {
};
