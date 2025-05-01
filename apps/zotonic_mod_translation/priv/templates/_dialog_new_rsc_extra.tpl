<div class="form-group">
    <label class="control-label" for="{{ #language }}">{_ Language _}</label>
    <select class="form-control" id="{{ #language }}" name="language">
        <option value="">{_ Automatic from title _}</option>
        <option disabled></option>
        {% for code, lang in m.translation.language_list_editable %}
            <option value="{{ code }}">{{ lang.name }}</option>
        {% endfor %}
    </select>
</div>
