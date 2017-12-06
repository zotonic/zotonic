<div class="form-group view-expanded survey-test-checkbox">
    <div class="checkbox">
        <label>
            <input type="checkbox" value="1" name="block-{{name}}-is_test" id="{{ #is_test }}" {% if blk.is_test %}checked{% endif %} />
            {_ Quiz or test question _}
        </label>
        {% wire id=#is_test
                action={script script="$(this).closest('.block').find('.test-controls').slideToggle();"}
        %}
    </div>

    <div class="checkbox">
        <label class="test-controls" {% if not blk.is_test %}style="display:none"{% endif %}>
            <input type="checkbox" value="1" name="block-{{name}}-is_test_direct" id="{{ #is_test_direct }}" {% if blk.is_test_direct %}checked{% endif %} />
            {_ Instant feedback (Learning Mode) _}
        </label>
    </div>

    <div class="checkbox">
        <label class="test-controls" {% if not blk.is_test %}style="display:none"{% endif %}>
            <input type="checkbox" value="1" name="block-{{name}}-is_test_neg" id="{{ #is_test_neg }}" {% if blk.is_test_neg %}checked{% endif %} />
            {_ Subtract points for wrong answers (total never less than 0) _}
        </label>
    </div>
</div>

<div class="form-group view-expanded test-controls" {% if not blk.is_test %}style="display:none"{% endif %}>
    <label class="control-label">{_ Points per answer _}</label>
    <div class="controls">
        <input type="text" name="block-{{name}}-test_points" id="{{ #test_points }}"
               class="input-small form-control" value="{{ blk.test_points }}" placeholder="1" />
        {% validate id=#test_points name="block-"++name++"-test_points"
                    type={numericality minimum=0}
        %}
    </div>
</div>
