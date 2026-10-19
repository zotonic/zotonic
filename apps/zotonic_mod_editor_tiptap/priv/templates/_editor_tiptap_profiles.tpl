{# JavaScript only. Override individual blocks using the overrules tag. #}
window.z_editor_tiptap_config = {
    ...window.z_editor_tiptap_config,
    default: {
        codeLanguages: {
            {% block code_languages %}
            erlang: 'Erlang', elixir: 'Elixir', javascript: 'JavaScript', typescript: 'TypeScript',
            html: 'HTML', css: 'CSS', scss: 'SCSS', json: 'JSON', yaml: 'YAML',
            sql: 'SQL', python: 'Python', bash: 'Shell', xml: 'XML', markdown: 'Markdown'
            {% endblock %}
        },
        ...window.z_editor_tiptap_config?.default
    },
    simple: {
        {% block simple %}
        toolbar: [
            'undo', 'redo', '|', 'bold', 'italic', '|',
            'bulletList', 'orderedList', '|', 'link', 'unlink', 'zlink'
        ]
        {% endblock %}
    },
    normal: {
        {% block normal %}
        toolbar: [
            'undo', 'redo', '|', 'format', 'bold', 'italic', 'underline', '|',
            'bulletList', 'orderedList', 'outdent', 'indent', 'blockquote', '|',
            'link', 'unlink', 'zlink', 'zmedia', '|', 'removeFormat', 'fullscreen'
        ]
        {% endblock %}
    },
    advanced: {
        {% block advanced %}
        toolbar: [
            'undo', 'redo', '|', 'format', 'bold', 'italic', 'underline', 'strike',
            'subscript', 'superscript', '|',
            'bulletList', 'orderedList', 'outdent', 'indent', 'blockquote', 'codeBlock', '|',
            'alignLeft', 'alignCenter', 'alignRight', 'ltr', 'rtl', '|',
            'link', 'unlink', 'zlink', 'zmedia', 'table', '|',
            'horizontalRule', 'hardBreak', 'removeFormat', 'fullscreen'
        ]
        {% endblock %}
    }
};
