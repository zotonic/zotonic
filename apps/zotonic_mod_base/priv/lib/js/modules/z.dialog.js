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
    $.extend({
        dialogAdd: function(options) {
            var width,
                $title,
                $body,
                $text,
                $modalContent,
                dialogClass,
                $modalDialog,
                $dialog;

            $('#zmodal').remove();
            $('.modal-backdrop').remove();

            options = $.extend({}, $.ui.dialog.defaults, options);

            $title = $('<div>')
              .addClass('modal-header')
              .append($('<a>')
              .addClass('close')
              .attr('data-dismiss', 'modal')
              .html('<span>&times;</span>'))
              .append($('<h4>')
              .addClass('modal-title')
              .html(options.title));
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
                $modalDialog.css({'width': width + 'px'});
            }

            $dialog = $('<div>')
              .attr('id', 'zmodal')
              .addClass(dialogClass)
              .append($modalDialog)
              .appendTo($('body'));

            $dialog
              .modal({backdrop: options.backdrop})
              .css({'overflow-x': 'hidden', 'overflow-y': 'auto'});


            if (options.center) {
                $modalDialog.hide();
                setTimeout(function() {
                    $.dialogCenter();
                    $modalDialog.show();
                }, 0);
            }

            if (typeof($.widgetManager) != 'undefined') {
                $dialog.widgetManager();
            }
            z_editor_add($dialog);
        },

        dialogClose: function() {
            $('#zmodal').modal('hide');
        },

        dialogRemove: function(obj) {
            obj = obj || $('#zmodal');
            z_editor_remove(obj);
            obj
              .draggable('destroy')
              .resizable('destroy')
              .fadeOut(300, function() {
                  $(this).remove();
              });
        },

        dialogCenter: function() {
            var $dialog,
                newMarginTop;
            $dialog = $('#zmodal:visible').find('.modal-dialog');
            newMarginTop = Math.max(0, ($(window).height() - $dialog.height()) / 2);
            newMarginTop *= .96; // visual coherence
            newMarginTop = Math.max(newMarginTop, 30);
            $dialog.css('margin-top', newMarginTop);
        },

        dialogScrollTo: function(position) {
            position = position || 0;
            $("#zmodal")[0].scrollTop = position
        }
    });

    $(window).on('resize', function() {
        $.dialogCenter();
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
                    backdrop: self.options.backdrop
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
    */
    $.ui.dialog.defaults = {
        title: 'Title',
        text: 'text',
        width: undefined,
        addclass: undefined,
        backdrop: 1,
        center: 1
    };
})(jQuery);
