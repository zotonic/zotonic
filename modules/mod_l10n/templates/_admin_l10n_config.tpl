<div class="control-group">
    <label for="app_id">{_ Timezone _}</label>
    <div class="controls">
        <select id="pref_tz" name="pref_tz">
            <option></option>
            {% include "_l10n_timezone_options.tpl" timezone=m.config.mod_l10n.timezone.value %}
        </select>
        {% wire id="pref_tz" 
            action={config_toggle module="mod_l10n" key="timezone"}
        %}

        <p class="help-block">
            {_ This sets the default timezone for this site. This timezone is used for the initial request by an user-agent or when the timezone is not known. _}
        </p>
    </div>
</div>
