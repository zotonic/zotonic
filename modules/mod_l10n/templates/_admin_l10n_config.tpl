<div class="form-group">
    <div>
        <select class="form-control" id="pref_tz" name="pref_tz">
            <option></option>
            {% include "_l10n_timezone_options.tpl" timezone=m.config.mod_l10n.timezone.value %}
        </select>
        {% wire id="pref_tz" 
            action={config_toggle module="mod_l10n" key="timezone"}
        %}

        <p class="help-block">
            {_ This sets the default timezone. This timezone is used for the initial request by an user-agent or when the timezone is not known. _}
        </p>
    </div>
</div>

<div class="form-group">
    <div class="checkbox">
        <label>
            <input type="checkbox" name="pref_tz_fixed" id="pref_tz_fixed" value="1"
                {% if m.config.mod_l10n.timezone_is_fixed.value %}checked{% endif %}
            />
            {_ Fix timezone, show all dates in the above timezone. _}
        </label>
    </div>
    {% wire id="pref_tz_fixed" 
        action={config_toggle module="mod_l10n" key="timezone_is_fixed"}
    %}

    <p class="help-block">
        {_ This forces the timezone to the configured timezone. Irrespective of the timezone selected by the user or user-agent. _}
    </p>
</div>
