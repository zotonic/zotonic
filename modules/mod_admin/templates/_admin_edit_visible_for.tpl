{% with m.rsc[id] as r %}
<div class="control-group">
	<label class="control-label" for="visible_for">{_ Visible for _}</label>
        <div class="controls">
	    <select id="visible_for" name="visible_for">
		<option value="0" {% ifequal 0 r.visible_for %}selected="selected"{% endifequal %}>{_ The whole world _}</option>
		<option value="1" {% ifequal 1 r.visible_for %}selected="selected"{% endifequal %}>{_ Community members _}</option>
		<option value="2" {% ifequal 2 r.visible_for %}selected="selected"{% endifequal %}>{_ Group members _}</option>
	    </select>
        </div>
</div>
{% endwith %}
