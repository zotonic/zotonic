<div class="col-md-12">
    <div class="checkbox">
        <label>
            <input value="1" type="checkbox"
                name="is_feature_show_address"
                {% if id.is_feature_show_address|if_undefined:true %}checked{% endif %}
            />
            {_ Show address on edit page _}
        </label>
    </div>
</div>
