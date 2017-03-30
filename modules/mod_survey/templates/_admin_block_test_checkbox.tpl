<div class="control-group view-expanded survey-test-checkbox">
    <label class="checkbox">
        <input type="checkbox" value="1" name="block-{{name}}-is_test" id="{{ #is_test }}" {% if blk.is_test %}checked{% endif %} />
        {_ Quiz or test question _}
    </label>
    {% wire id=#is_test 
            action={script script="$(this).closest('.block').find('.test-controls').slideToggle();"}
    %}

    <label class="checkbox test-controls" {% if not blk.is_test %}style="display:none"{% endif %}>
        <input type="checkbox" value="1" name="block-{{name}}-is_test_direct" id="{{ #is_test_direct }}" {% if blk.is_test_direct %}checked{% endif %} />
        {_ Direct feedback on selecting answer _}
    </label>

    <label class="checkbox test-controls" {% if not blk.is_test %}style="display:none"{% endif %}>
        <input type="checkbox" value="1" name="block-{{name}}-is_test_neg" id="{{ #is_test_neg }}" {% if blk.is_test_neg %}checked{% endif %} />
        {_ Subtract points for wrong answers _}
    </label>

</div>

<div class="control-group view-expanded test-controls" {% if not blk.is_test %}style="display:none"{% endif %}>
    <label class="control-label">{_ Points per answer _}</label>
    <div class="controls">
        <input type="text" name="block-{{name}}-test_points" id="{{ #test_points }}" 
               class="input-small" value="{{ blk.test_points }}" placeholder="1" />
        {% validate id=#test_points name="block-"++name++"-test_points"
                    type={numericality minimum=0}
        %}
    </div>
</div>
