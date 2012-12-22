<div class="controls">
	<label class="checkbox">
        <input value="1" type="checkbox"
               name="feature_show_address"
               {% if id.feature_show_address|if_undefined:`true` %}checked{% endif %}
               />
        {_ Show address on edit page _}
    </label>
</div>
