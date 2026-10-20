{# Include once in the outer editor; keep the iframe outside dynamically replaced forms. #}
<aside id="admin-edit-preview" class="admin-edit-preview" hidden aria-label="{_ Live view _}" data-preview-language="{{ z_language|escape }}">
    <div class="admin-edit-preview-resizer" data-admin-preview-resizer role="separator"
         tabindex="0" aria-label="{_ Resize Live view _}" aria-orientation="vertical"
         aria-controls="admin-edit-preview" aria-valuemin="30" aria-valuemax="60" aria-valuenow="40"></div>
    <div class="admin-edit-preview-toolbar">
        <div class="admin-edit-preview-info">
            <strong><span class="admin-edit-preview-language" data-admin-preview-language
                title="{_ Editing language _}" aria-live="polite"></span></strong>
            <span class="admin-edit-preview-dirty" data-admin-preview-dirty hidden aria-live="polite">{_ Click "Save" to see your changes. _}</span>
        </div>
        <button type="button" class="btn btn-default btn-sm" data-admin-preview="refresh">{_ Refresh _}</button>
        <a class="btn btn-default btn-sm" data-admin-preview-link target="_blank" rel="noopener">{_ Open in new tab _}</a>
        <button type="button" class="btn btn-default btn-sm" data-admin-preview="close" aria-label="{_ Close Live view _}">&times;</button>
    </div>
    <p class="admin-edit-preview-status" data-admin-preview-unavailable hidden>{_ This page cannot be previewed here. Use “Open in new tab” if available. _}</p>
    <iframe title="{_ Live view _}" sandbox="allow-same-origin allow-scripts allow-forms allow-popups allow-popups-to-escape-sandbox allow-downloads"></iframe>
</aside>
{% lib "js/admin-edit-preview.js" %}
