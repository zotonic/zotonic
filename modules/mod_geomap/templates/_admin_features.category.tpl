<div class="controls">
	<label class="checkbox">
        <input value="1" type="checkbox"
               name="feature_show_geodata"
               {% if id.feature_show_geodata|if_undefined:`true` %}checked{% endif %}
               />
        {_ Show geo data on edit page _}
    </label>
</div>
