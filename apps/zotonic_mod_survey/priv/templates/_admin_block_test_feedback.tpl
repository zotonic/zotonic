<div class="test-controls" {% if not blk.is_test %}style="display:none"{% endif %}>
    <div class="form-group label-floating">
        <textarea id="block-{{name}}-test_correct--{{ lang_code }}" name="blocks[].test_correct${{ lang_code }}" class="form-control" rows="2" placeholder="{_ Feedback if correct _} ({{ lang_code }})">{{ blk.test_correct|translation:lang_code }}</textarea>
        <label class="control-label">{_ Feedback if correct _} ({{ lang_code }})</label>
    </div>

    <div class="form-group label-floating">
        <textarea id="block-{{name}}-test_wrong--{{ lang_code }}" name="blocks[].test_wrong${{ lang_code }}" class="form-control" rows="2" placeholder="{_ Feedback if wrong _} ({{ lang_code }})">{{ blk.test_wrong|translation:lang_code }}</textarea>
        <label class="control-label">{_ Feedback if wrong _} ({{ lang_code }})</label>
    </div>
</div>
