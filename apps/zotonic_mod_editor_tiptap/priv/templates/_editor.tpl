{# Self-contained editor integration; no mod_editor_tinymce templates or assets. #}
{% block _editor %}
{% lib "css/editor-tiptap.css" "css/editor-tiptap-body.css" %}
{% all include "_editor_tiptap_head.tpl" %}
{% lib "js/zotonic-editor-tiptap.js" %}

{% wire name="zmedia" action={dialog_open
    intent="select" template="_action_dialog_connect.tpl"
    title=_"Insert media" width="large" subject_id=id predicate=`depiction`
    is_zmedia tab="depiction" callback="window.z_editor_tiptap.mediaDone"
    center=0 level=5 autoclose
    tabs_enabled=zmedia_tabs_enabled tabs_disabled=zmedia_tabs_disabled|default:["new"]
} %}
{% wire name="zlink" action={dialog_open
    intent="select" template="_action_dialog_connect.tpl"
    title=_"Add link" width="large" subject_id=id
    is_zlink tab="find" callback="window.z_editor_tiptap.linkDone"
    center=0 level=5 autoclose
    tabs_enabled=zlink_tabs_enabled tabs_disabled=zlink_tabs_disabled
} %}

{% javascript %}
    window.z_editor_tiptap.configure({
        mediaPreviewUrl: "{% url admin_media_preview id="__ID__" %}",
        labels: {
            toolbar: "{{ _"Text formatting"|escapejs }}",
            undo: "{{ _"Undo"|escapejs }}", redo: "{{ _"Redo"|escapejs }}",
            format: "{{ _"Format"|escapejs }}", paragraph: "{{ _"Paragraph"|escapejs }}",
            h1: "{{ _"Heading 1"|escapejs }}", h2: "{{ _"Heading 2"|escapejs }}",
            h3: "{{ _"Heading 3"|escapejs }}", h4: "{{ _"Heading 4"|escapejs }}",
            h5: "{{ _"Heading 5"|escapejs }}", h6: "{{ _"Heading 6"|escapejs }}",
            bold: "{{ _"Bold"|escapejs }}", italic: "{{ _"Italic"|escapejs }}",
            underline: "{{ _"Underline"|escapejs }}", strike: "{{ _"Strikethrough"|escapejs }}",
            subscript: "{{ _"Subscript"|escapejs }}", superscript: "{{ _"Superscript"|escapejs }}",
            bulletList: "{{ _"Bullet list"|escapejs }}", orderedList: "{{ _"Numbered list"|escapejs }}",
            indent: "{{ _"Indent"|escapejs }}", outdent: "{{ _"Outdent"|escapejs }}",
            blockquote: "{{ _"Quote"|escapejs }}", codeBlock: "{{ _"Code block"|escapejs }}",
            alignLeft: "{{ _"Align left"|escapejs }}", alignCenter: "{{ _"Align center"|escapejs }}",
            alignRight: "{{ _"Align right"|escapejs }}",
            ltr: "{{ _"Left to right"|escapejs }}", rtl: "{{ _"Right to left"|escapejs }}",
            horizontalRule: "{{ _"Horizontal line"|escapejs }}", hardBreak: "{{ _"Line break"|escapejs }}",
            link: "{{ _"Link"|escapejs }}", unlink: "{{ _"Remove link"|escapejs }}",
            zlink: "{{ _"Insert internal link"|escapejs }}", zmedia: "{{ _"Insert or edit media"|escapejs }}",
            mediaProperties: "{{ _"Media Properties"|escapejs }}",
            removeFormat: "{{ _"Remove formatting"|escapejs }}", fullscreen: "{{ _"Full window"|escapejs }}",
            table: "{{ _"Table"|escapejs }}", insertTable: "{{ _"Insert table"|escapejs }}",
            addRowAfter: "{{ _"Add row"|escapejs }}", addColumnAfter: "{{ _"Add column"|escapejs }}",
            deleteRow: "{{ _"Delete row"|escapejs }}", deleteColumn: "{{ _"Delete column"|escapejs }}",
            toggleHeaderRow: "{{ _"Toggle header row"|escapejs }}", mergeCells: "{{ _"Merge cells"|escapejs }}",
            splitCell: "{{ _"Split cell"|escapejs }}", deleteTable: "{{ _"Delete table"|escapejs }}",
            url: "{{ _"URL"|escapejs }}", invalidUrl: "{{ _"Enter a valid link."|escapejs }}",
            apply: "{{ _"Apply"|escapejs }}", cancel: "{{ _"Cancel"|escapejs }}",
            required: "{{ _"This field is required."|escapejs }}"
        }
    });
    {% all include tiptap_overrides_tpl|default:"_editor_tiptap_overrides_js.tpl" id=id %}
    {% if not is_editor_include %}z_editor_init();{% endif %}
{% endjavascript %}
{% endblock %}
