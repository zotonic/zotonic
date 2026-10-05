{% with tree_id|default:id as translation_tree_id %}
{% if translation_tree_id.is_editable %}
    {% lib "js/z.translation-tree.js" %}
    {% wire name="translation-tree-dialog" postback={dialog id=translation_tree_id} delegate=`m_translation_tree` %}
    {% javascript %}
        window.z_translation_tree.watch({{ translation_tree_id }}, {
            title: "{_ Translation in progress _}",
            ready: "{_ Ready _}",
            removing: "{_ Removing language _}",
            blocked: "{_ Editing is disabled until this operation finishes. _}",
            done: "{_ The operation has finished. Reload the page to continue editing. _}",
            failed: "{_ The operation could not be completed. Some pages may have been changed. _}",
            skipped: "{_ Skipped _}",
            errors: "{_ Failed _}",
            reload: "{_ Reload page _}",
            error: "{_ Could not start the operation. Check your permissions and language options, or try again later. _}",
            dirty: "{_ Save or discard your changes before translating the tree. _}",
            confirmTree: "{_ Apply the selected translation options to {count} pages? Editing will be disabled until the operation finishes. _}",
            confirm: "{_ This permanently removes the selected language from every page in this tree. Pages with only that language will be kept unchanged. This cannot be undone. Continue? _}"
        }, JSON.parse("{{ m.translation_tree.status[translation_tree_id]|to_json|escapejs }}"));
    {% endjavascript %}
{% endif %}
{% endwith %}
