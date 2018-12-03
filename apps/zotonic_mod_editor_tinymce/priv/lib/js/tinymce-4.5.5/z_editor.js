/*jslint browser: true*/
/*global jQuery, tinymce, tinyInit, z_on_visible*/

'use strict';

var z_editor = (function ($) {

    var CLASS_EDITOR = 'z_editor',
        CLASS_EDITOR_INSTALLED = 'z_editor-installed',
        initEditors,
        instances,
        initEditor,
        addEditor,
        removeEditor;

    initEditors = function(className) {
        $('.' + className + ':visible').each(function () {
            addEditor($(this));
        }).removeClass(className).addClass(CLASS_EDITOR);
    };

    instances = function($el, installed) {
        if (installed && $el.hasClass(CLASS_EDITOR_INSTALLED)) {
            return $el;
        } else if (!installed && $el.hasClass(CLASS_EDITOR)) {
            return $el;
        } else {
            var editorClass = installed ? CLASS_EDITOR_INSTALLED : CLASS_EDITOR;
            return $el.find('textarea.' + editorClass);
        }
    };

    initEditor = function($el) {
        var id, options;
        id = $el.attr('id');
        options = $.extend({}, tinyInit || {});
        options.selector = '#' + id;
        if ($el.attr('dir')) {
            options.directionality = $el.attr('dir');
        }
        tinymce.init(options);
        $el.addClass(CLASS_EDITOR_INSTALLED);
    };

    addEditor = function($el) {
        var id = $el.attr('id');
        tinymce.remove('#'+id);
        if (id) {
            z_on_visible('#' + id, function() {
                initEditor($el);
            });
        } else {
            setTimeout(function () {
                initEditor($el);
            }, 200);
        }
    };

    removeEditor = function($el) {
        var mceId = '#' + $el.attr('id');
        tinymce.remove(mceId);
        $el.removeClass(CLASS_EDITOR_INSTALLED);
    };

    return {
        init: function () {
            initEditors('z_editor-init');
            initEditors('tinymce-init'); // tinymce-init might be still in use in code
        },

        add: function ($el) {
            instances($el, false).each(function () {
                addEditor($(this));
            });
        },

        save: function () {
            tinymce.triggerSave(); // calls save on all instances
        },

        remove: function ($el) {
            instances($el, true).each(function () {
                removeEditor($(this));
            });
        }
    };
}(jQuery));
