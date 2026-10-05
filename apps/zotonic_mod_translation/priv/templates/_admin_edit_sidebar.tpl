{# Admin edit sidebar item holding the checkboxes with the current languages     #}
{# This is invisible as the central language-tab is used to add/remove languages #}
<div style="display: none">
    {% include "_translation_edit_languages.tpl" %}
</div>

{% if id.is_a.collection or id.is_a.menu %}
    <div class="widget">
        <h3 class="widget-header">{_ Translate _}</h3>
        <div class="widget-content">
            {% include "_translation_tree_button.tpl" tree_id=id %}
        </div>
    </div>
{% endif %}
