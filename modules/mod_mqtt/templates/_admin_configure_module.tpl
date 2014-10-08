<div class="modal-body">
    <div class="control-group">
        <div>
            {% wire id="pubsubdbg" 
                action={config_toggle module="mod_mqtt" key="debug_pubsub"}
            %}
            <label class="checkbox inline">
                <input type="checkbox" id="pubsubdbg" value="1" {% if m.config.mod_mqtt.debug_pubsub.value %}checked="checked"{% endif %} />
                {_ Debug MQTT publish and subscribe actions in the Zotonic console _}
            </label>
        </div>
    </div>
</div>

<div class="modal-footer">
    {% button class="btn" text=_"Close" action={dialog_close} tag="a" %}
</div>

