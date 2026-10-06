{% with tree_id|default:id as translation_tree_id %}
{% if is_bulk or translation_tree_id.is_editable %}
    {% lib "js/z.translation-tree.js" %}
    {% wire name="translation-tree-dialog" postback={dialog id=translation_tree_id} delegate=`m_translation_tree` %}
    {% javascript %}
        window.z_translation_tree.watch("{{ translation_tree_id|escapejs }}", {
            title: "{_ Translation in progress _}",
            ready: "{_ Ready _}",
            removing: "{_ Removing language _}",
            blocked: "{_ Editing is disabled until translation finishes. _}",
            done: "{_ Translation has finished. Reload the page to continue editing. _}",
            failed: "{_ Translation could not be completed. Some pages may have been changed. _}",
            skipped: "{_ Skipped _}",
            errors: "{_ Failed _}",
            reload: "{_ Reload page _}",
            error: "{_ Could not start the translation. Check your permissions and language options, or try again later. _}",
            dirty: "{_ Save or discard your changes before starting the translation. _}",
            confirmTree: "{_ Start the translation of {count} pages? Editing will be disabled until the translations are ready. _}",
            confirm: "{% if is_bulk %}{_ This permanently removes the selected language from every selected page. Pages with only that language will be kept unchanged. This cannot be undone. Continue? _}{% else %}{_ This permanently removes the selected language from every page in this tree. Pages with only that language will be kept unchanged. This cannot be undone. Continue? _}{% endif %}"
        }, JSON.parse("{{ m.translation_tree.status[translation_tree_id]|to_json|escapejs }}"));
    {% endjavascript %}
{% endif %}
{% endwith %}
