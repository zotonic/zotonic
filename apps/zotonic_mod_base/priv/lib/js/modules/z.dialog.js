/* dialog js
 ----------------------------------------------------------

 @package:      Zotonic 2009, 2012, 2015
 @Author:       Tim Benniks <tim@timbenniks.nl>

 Copyright 2009 Tim Benniks
 Copyright 2012 Arjan Scherpenisse
 Copyright 2015, 2016 Arthur Clemens

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

    function dialogLevel($dialog) {
        const level = $dialog.attr('data-modal-level');
        if (typeof level !== 'undefined') {
            return parseInt(level);
        } else {
            return 0;
        }
    }

    function dialogTopLevel() {
        let maxLevel = undefined;
        $(".modal").each(function() {
            if ($(this).is(":visible")) {
                maxLevel = Math.max(maxLevel ?? 0, dialogLevel($(this)));
            }
        });
        return maxLevel;
    }

    function topDialog() {
        let maxLevel = 0;
        let dialog = undefined;
        $(".modal").each(function() {
            if ($(this).is(":visible")) {
                const lev = dialogLevel($(this));
                if (lev >= maxLevel) {
                    maxLevel = lev;
                    dialog = $(this);
                }
            }
        });
        return dialog;
    }

    $.extend({
        dialogAdd: function(options) {
            var width,
                level,
                $title,
                $body,
                $text,
                $modalContent,
                dialogClass,
                $modalDialog,
                $dialog;

            options = $.extend({}, $.ui.dialog.defaults, options);

            level = options.level ?? 0;
            if (level === "top") {
                let topLevel = dialogTopLevel();
                if (typeof topLevel == 'undefined') {
                    level = 0;
                } else {
                    level = topLevel + 1;
                }
            } else {
                $(".modal").each(function() {
                    if ($(this).is(":visible") && dialogLevel($(this)) >= level) {
                        $(this).modal('hide');
                    }
                });
            }

            if (options.backdrop !== 'static') {
              $title = $('<div>')
                .addClass('modal-header')
                .append($('<a>')
                .addClass('close')
                .attr('data-dismiss', 'modal')
                .html('<span>&times;</span>'))
                .append($('<h4>')
                .addClass('modal-title')
                .html(options.title));
            } else {
              $title = $('<div>')
                .addClass('modal-header')
                .append($('<h4>')
                .addClass('modal-title')
                .html(options.title));
            }
            $modalContent = $('<div>').addClass('modal-content');
            $text = $('<div>').html(options.text);

            // if .modal-body is used in a template, don't add it again
            if ($text.hasClass('modal-body')) {
                $body = $text;
            } else {
                $body = $('<div>')
                  .addClass('modal-body')
                  .html(options.text);
            }

            $modalContent = $('<div>')
              .addClass('modal-content')
              .append($title)
              .append($body);

            dialogClass = 'modal';
            if (typeof(options.addclass) == 'string') {
                dialogClass += ' ' + options.addclass;
            }

            $modalDialog = $('<div>')
              .addClass('modal-dialog')
              .append($modalContent);

            width = options.width;
            if (width) {
                if (width === 'large') {
                    $modalDialog.addClass('modal-lg');
                } else if (width === 'small') {
                    $modalDialog.addClass('modal-sm');
                } else {
                    $modalDialog.css({'width': width + 'px'});
                }
            }

            const id = (level == 0) ? "zmodal" : ("zmodal-" + level);
            $dialog = $('<div>')
              .attr('id', id)
              .attr('data-modal-level', level)
              .css({ 'z-index': 1000 * (level+1) + 50})
              .addClass(dialogClass)
              .append($modalDialog)
              .appendTo($('body'));

            $dialog
              .modal({backdrop: options.backdrop, keyboard: options.keyboard ?? true})
              .css({'overflow-x': 'hidden', 'overflow-y': 'auto'});

            if (options.center) {
                $modalDialog.hide();
                setTimeout(function() {
                    $.dialogCenter($modalDialog);
                    $modalDialog.show();
                }, 0);
            }

            if (typeof($.widgetManager) != 'undefined') {
                $dialog.widgetManager();
            }
            z_editor_add($dialog);

            $dialog.on('hidden.bs.modal', () => $.dialogRemove($dialog));
        },

        dialogClose: function(options) {
            let level;
            if (options && options.level) {
                if (options.level === "top") {
                    level = dialogTopLevel();
                } else {
                    level = options.level;
                }
            } else {
                level = dialogTopLevel();
            }
            if (typeof level !== 'undefined') {
                $(".modal").each(function() {
                    if ($(this).is(":visible") && dialogLevel($(this)) >= level) {
                        $(this).modal('hide');
                    }
                });
            }
        },

        // Called after the dialog is hidden.
        dialogRemove: function(obj) {
            if (obj) {
                z_editor_remove(obj);
                obj.remove();
            }
        },

        dialogCenter: function($modalDialog) {
            $modalDialog = $modalDialog || $("#zmodal");
            let newMarginTop = Math.max(0, ($(window).height() - $modalDialog.height()) / 2);
            newMarginTop *= 0.96; // visual coherence
            newMarginTop = Math.max(newMarginTop, 30);
            $modalDialog.css('margin-top', newMarginTop);
        },

        dialogScrollTo: function(position) {
            const $dialog = topDialog();
            position = position || 0;
            if ($dialog) {
                $dialog[0].scrollTop = position;
            }
        }
    });

    $(window).on('resize', function() {
        $(".dialog:visible .modal-dialog").each(function() {
            $.dialogCenter($(this));
        })
    });

    $.widget('ui.show_dialog', {
        _init: function() {
            var self = this;
            this.element.click(function() {
                $.dialogAdd({
                    title: self.options.title,
                    text: self.options.text,
                    width: self.options.width,
                    addclass: self.options.addclass,
                    backdrop: self.options.backdrop,
                    keyboard: self.options.keyboard,
                    level: self.options.level
                });
            });
        }
    });

    /*
    Default dialog parameters:
    title: text, will be inserted in h4
    text: text content, may contain html (will be inserted into div)
    width: (optional)
    addclass: (optional) classname will be appended to default dialog class
    backdrop: (optional) boolean (0, 1) or the string 'static'
    center: (optional) boolean (0, 1); set to 0 to align dialog to the top
    level: (optional) the nesting level of the dialog
    */
    $.ui.dialog.defaults = {
        title: 'Title',
        text: 'text',
        width: undefined,
        addclass: undefined,
        backdrop: true,
        center: true,
        keyboard: true,
        level: 0
    };
})(jQuery);
