<div class="form-group view-expanded survey-test-checkbox">
    <div class="checkbox">
        <label>
            <input type="checkbox" value="1" name="blocks[].is_test" id="{{ #is_test }}" {% if blk.is_test %}checked{% endif %} />
            {_ Quiz or test question _}
        </label>
        {% wire id=#is_test
                action={script script="$(this).closest('.block').find('.test-controls').slideToggle();"}
        %}
    </div>

    <div class="checkbox">
        <label class="test-controls" {% if not blk.is_test %}style="display:none"{% endif %}>
            <input type="checkbox" value="1" name="blocks[].is_test_direct" id="{{ #is_test_direct }}" {% if blk.is_test_direct %}checked{% endif %} />
            {_ Instant feedback (Learning Mode) _}
        </label>
    </div>
</div>
