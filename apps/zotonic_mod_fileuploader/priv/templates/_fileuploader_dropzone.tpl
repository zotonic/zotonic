{# Optional admin integration. The native picker and drops share the same handler. #}
<div id="{{ #dropzone }}" class="fileuploader-dropzone"
     data-fileuploader="{{ upload_options|escape }}"
     {% if single %}data-fileuploader-single{% endif %}
     {% if replace %}data-fileuploader-confirm="{_ Replace the current media file? _}"{% endif %}>
    <label>
        <strong>{{ label|escape }}</strong>
        <span class="fileuploader-drag-help">{_ Drop files here or choose files. _}</span>
        <input type="file" class="form-control nosubmit" {% if not single %}multiple{% endif %}>
    </label>
    <p class="fileuploader-status" role="status" aria-live="polite"></p>
</div>
