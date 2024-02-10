<div class="form-group">
    <label class="control-label" for="{{ #medium_language }}">
        {_ Language of media content _}
        <a href="javascript:void(0)" class="z-btn-help do_dialog"
           data-dialog="title: '{_ Language of media content _}', text: '{{ _'Some images, PDFs, audio and video are presented in a single language. Here you can select this language.'|escapejs }}', level: 10"
           title="{_ Help _}"></a>
    </label>
    <select id="{{ #medium_language }}" name="medium_language" class="form-select" style="width: auto">
        <option value="">{_ Select _}</option>
        {% for code, lang in m.translation.language_list_editable %}
            <option value="{{ code }}" {% if id.medium_language == code %}selected{% endif %}>
                {{ lang.name }}
            </option>
        {% endfor %}
    </select>
</div>
