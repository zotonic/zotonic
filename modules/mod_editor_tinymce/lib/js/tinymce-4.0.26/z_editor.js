var z_editor = (function ($) {
    "use strict";

    var initEditor;

    initEditor = function(className) {
        $("." + className + ":visible").each(function () {
            var $elt = $(this),
                id = $elt.attr('id'),
                ti,
                f;

            f = function () {
                ti = $.extend({}, tinyInit);
                if ($elt.attr("dir")) {
                    ti.directionality = $elt.attr("dir");
                }
                $elt.tinymce(ti).addClass('z_editor-installed');
            };
            if (id) {
                z_on_visible("#"+id, f);
            } else {
                setTimeout(f, 200);
            }
        }).removeClass(className).addClass("z_editor");
    };

    return {
        init: function () {
            initEditor("z_editor-init");
            // tinymce-init might be still in use in code
            initEditor("tinymce-init");
        },

        add: function ($el) {
            var self = this;

            $("textarea.z_editor", $el).each(function () {
                var $elt = $(this),
                    id = $elt.attr('id');

                if (id) {
                    z_on_visible('#'+id, function() {
                        self.initElement($elt);
                    });
                } else {
                    setTimeout(function () {
                        self.initElement($elt);
                    }, 200);
                }
            });
        },

        initElement: function ($elt) {
            if (typeof $elt.tinymce === "function") {
                var ti = $.extend({}, tinyInit || {});
                if ($elt.attr("dir")) {
                    ti.directionality = $elt.attr("dir");
                }
                $elt.tinymce(ti);
            } else if (typeof tinyMCE === "object") {
                var mce_id = $elt.attr("id");
                tinyMCE.execCommand("mceAddControl", false, mce_id);
            }
            $elt.addClass('z_editor-installed');
        },

        save: function ($el) {
            var tiny = $("textarea.z_editor-installed", $el);
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
            $("textarea.z_editor-installed", $el).each(function () {
                if (tinyMCE !== undefined) {
                    tinyMCE.execCommand("mceRemoveControl", false, $(this).attr("id"));
                } else if (typeof $(this).tinymce === "function") {
                    $(this).tinymce().remove();
                }
                $(this).removeClass('z_editor-installed');
            });
        }
    };
}(jQuery));