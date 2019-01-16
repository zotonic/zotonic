/* dialog js
 ----------------------------------------------------------

 @package:      Zotonic 2009, 2012
 @Author:       Tim Benniks <tim@timbenniks.nl>

 Copyright 2009 Tim Benniks
 Copyright 2012 Arjan Scherpenisse

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
        // center the dialog vertically again
        dialogReposition: function() {
            var $dialog = $("#zmodal:visible");
            if ($dialog.length) {
              if ($(window).width() >= 768 && $dialog.height() > 0.79 * $(window).height()) {
                  $dialog.addClass('high');
              } else {
                  var self = this;
                  setTimeout(function() { self.dialogReposition(); }, 250);
              }
            }
        },

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
            $(".modal-backdrop").remove();

            options = $.extend({}, $.ui.dialog.defaults, options);
            width = options.width;

            if (options.backdrop !== 'static') {
                $title = $("<div>")
                  .addClass("modal-header")
                  .append($("<a>")
                  .addClass("close")
                  .attr("data-dismiss", "modal")
                  .html("&times;"))
                  .append($("<h4>")
                  .addClass("modal-title")
                  .html(options.title));
            } else {
                $title = $("<div>")
                  .addClass("modal-header")
                  .append($("<h4>")
                  .addClass("modal-title")
                  .html(options.title));
            }
            $modalContent = $("<div>").addClass("modal-content");
            $text = $("<div>").html(options.text);

            // if .modal-body is used in a template, don't add it again
            if ($text.hasClass("modal-body")) {
                $body = $text;
            } else {
                $body = $("<div>")
                  .addClass("modal-body")
                  .html($text);
            }
            
            $modalContent = $("<div>")
              .addClass("modal-content")
              .append($title)
              .append($body);
            
            dialogClass = 'modal';
            if (typeof(options.addclass) == "string") {
                dialogClass += ' ' + options.addclass;
            }

            $modalDialog = $("<div>")
              .addClass("modal-dialog")
              .append($modalContent);

            $dialog = $("<div>")
              .attr("id", "zmodal")
              .addClass(dialogClass)
              .append($modalDialog)
              .appendTo($("body"));

            $dialog
              .modal({backdrop: options.backdrop})
              .css({"overflow-x": "hidden", "overflow-y": "auto"});

            if (width > 0) {
                $dialog.css({
                    width: width,
                    'margin-left': function () {
                        return -($(this).width() / 2);
                    }
                });
            }

            this.dialogReposition();

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
        }
    });
 
    $.widget("ui.show_dialog", {
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
 
    $.ui.dialog.defaults = {
        title: 'Title',
        text: 'text',
        width: undefined,
        addclass: undefined,
        backdrop: 1,
        center: 1
    };
})(jQuery);
