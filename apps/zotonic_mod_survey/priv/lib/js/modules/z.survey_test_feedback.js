/* z.survey_test_feedback js
----------------------------------------------------------

Copyright 2017-2022 Marc Worrell

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
                let name = $(this).attr('name');
                let $wrap = $(this).closest('.controls');
                if ($wrap.length == 0) {
                    if ($(this).attr('type') == 'radio') {
                        $wrap = $(this).closest('div').find('label');
                    } else {
                        $wrap = $(this).closest('label');
                    }
                }
                self.reset_feedback_icons($wrap);
                $wrap.find("input[name=" + name + "]").each(
                    function(_idx, input) {
                        let nr = $(input).attr('data-answer-nr');
                        if ($(input).is(':checked')) {
                            let $icon_wrap = $(input).closest('label');
                            self.show_feedback($(input), obj, $icon_wrap, 'true');
                            $wrap.find(".survey-test-feedback-answer[data-answer-nr=\""+ nr +"\"").show();
                        }
                    });
            });

        $(obj).on(
            'change',
            'select',
            function(ev) {
                let $select = $(this);
                let val = $select.val();
                let $wrap = $select.closest('.controls');
                if ($wrap.length == 0) {
                    $wrap = $select.closest('div');
                }
                self.reset_feedback_icons($wrap);
                $(this).find('option').each(
                    function(_, option) {
                        if ($(option).attr('value') == val) {
                            self.show_feedback($(option), obj, $wrap, 'true', $select);
                            let nr = $(option).attr('data-answer-nr');
                            $wrap.find(".survey-test-feedback-answer[data-answer-nr=\""+ nr +"\"").show();
                        }
                    });
            });
    },

    reset_feedback_icons: function ($wrap) {
        $wrap
            .removeClass("survey-test-feedback")
            .removeClass("survey-q-ok")
            .removeClass("survey-q-not-ok")
            .find(".survey-test-feedback-icon")
            .remove();
        $wrap
            .find(".survey-test-feedback-answer")
            .hide();
    },

    show_feedback: function ($elt, obj, $icon_wrap, value, after) {
        let cls = 'survey-q-ok';
        let icn = 'fa fa-check';

        if ($elt.attr('data-is-correct') != value) {
            cls = 'survey-q-not-ok';
            icn = 'fa fa-times';
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
            .addClass(cls);

        const icon = "<span class='survey-test-feedback-icon'><span class='"+icn+"'></span></span>";
        if (after) {
            $(icon).insertAfter(after);
        } else {
            $icon_wrap.append(icon);
        }
    }
});

$.ui.survey_test_feedback.defaults = {
};
