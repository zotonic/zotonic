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
        removeEditor,
        currentTheme,
        currentThemePreference,
        setEditorTheme,
        syncEditorsTheme,
        observeThemeChanges,
        themeObserver;

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

    currentTheme = function() {
        return document.documentElement.getAttribute('data-bs-theme') === 'dark'
            ? 'dark'
            : 'light';
    };

    currentThemePreference = function() {
        var theme = document.documentElement.getAttribute('data-zotonic-theme');
        return theme === 'light' || theme === 'dark' || theme === 'auto'
            ? theme
            : currentTheme();
    };

    setEditorTheme = function(editor) {
        var doc;
        try {
            doc = editor.getDoc && editor.getDoc();
            if (doc && doc.documentElement) {
                doc.documentElement.setAttribute('data-bs-theme', currentTheme());
                doc.documentElement.setAttribute('data-zotonic-theme', currentThemePreference());
            }
        } catch (e) {
            // Ignore editors that are not fully initialized yet.
        }
    };

    syncEditorsTheme = function() {
        var i;
        if (window.tinymce && tinymce.editors && tinymce.editors.length) {
            for (i = 0; i < tinymce.editors.length; i += 1) {
                setEditorTheme(tinymce.editors[i]);
            }
        }
    };

    observeThemeChanges = function() {
        if (!themeObserver && window.MutationObserver) {
            themeObserver = new MutationObserver(syncEditorsTheme);
            themeObserver.observe(document.documentElement, {
                attributes: true,
                attributeFilter: ['data-bs-theme', 'data-zotonic-theme']
            });
        }
    };

    initEditor = function($el) {
        var id, options, setup, initInstanceCallback;
        id = $el.attr('id');
        options = $.extend({}, tinyInit || {});
        options.selector = '#' + id;
        options.branding = false;
        if ($el.attr('dir')) {
            options.directionality = $el.attr('dir');
        }
        setup = options.setup;
        options.setup = function(editor) {
            if (typeof setup === 'function') {
                setup.call(this, editor);
            }
            editor.on('init', function() {
                setEditorTheme(editor);
            });
        };
        initInstanceCallback = options.init_instance_callback;
        options.init_instance_callback = function (editor) {
            setEditorTheme(editor);
            editor.on('Change', function (e) {
                $el.closest('form').trigger('z:editorChange');
            });
            if (typeof initInstanceCallback === 'function') {
                initInstanceCallback.call(this, editor);
            }
        }
        observeThemeChanges();
        tinymce.init(options);
        $el.addClass(CLASS_EDITOR_INSTALLED);
    };

    addEditor = function($el) {
        var id = $el.attr('id');
        if (id) {
            z_on_visible('#' + id, function() {
                tinymce.remove('#'+id);
                initEditor($el);
            });
        } else {
            setTimeout(function () {
                tinymce.remove('#'+id);
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
