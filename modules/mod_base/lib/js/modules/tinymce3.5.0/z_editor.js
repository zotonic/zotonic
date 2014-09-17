var z_editor = (function ($) {
    "use strict";

    var initEditor;

    initEditor = function(className) {
        $("." + className + ":visible").each(function () {
            var self = $(this),
                ti;
            setTimeout(function () {
                ti = $.extend({}, tinyInit);
                if (self.attr("dir")) {
                    ti.directionality = self.attr("dir");
                }
                self.tinymce(ti);
            }, 200);
        }).removeClass(className).addClass("z_editor");
    };

    return {
        init: function () {
            initEditor("z_editor-init");
            // tinymce-init might be still in use in code
            initEditor("tinymce-init");
        },

        add: function ($el) {
            $("textarea.z_editor,textarea.tinymce", $el).each(function () {
                if (typeof $(this).tinymce === "function") {
                    var self = $(this);
                    setTimeout(function () {
                        if (typeof tinyInit === "object") {
                            self.tinymce(tinyInit);
                        } else {
                            self.tinymce({});
                        }
                    }, 200);
                } else if (typeof tinyMCE === "object") {
                    var mce_id = $(this).attr("id");
                    setTimeout(function () {
                        tinyMCE.execCommand("mceAddControl", false, mce_id);
                    }, 200);
                }
            });
        },

        save: function ($el) {
            var tiny = $("textarea.z_editor,textarea.tinymce", $el);
            if (tiny.length > 0) {
                if (typeof tiny.tinymce === "function") {
                    tiny.each(function () {
                        $(this).tinymce().save();
                    });
                } else if (typeof tinyMCE === "object") {
                    tinyMCE.triggerSave(true, true);
                }
            }
        },

        remove: function ($el) {
            $("textarea.z_editor,textarea.tinymce", $el).each(function () {
                if (tinyMCE !== undefined) {
                    tinyMCE.execCommand("mceRemoveControl", false, $(this).attr("id"));
                } else if (typeof $(this).tinymce === "function") {
                    $(this).tinymce().remove();
                }
            });
        }
    };
}(jQuery));