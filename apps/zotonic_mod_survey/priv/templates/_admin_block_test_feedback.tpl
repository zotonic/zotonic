<div class="test-controls" {% if not blk.is_test %}style="display:none"{% endif %}>
    <div class="form-group label-floating">
        <input type="text" id="block-{{name}}-test_correct--{{ lang_code }}" name="blocks[].test_correct${{ lang_code }}"
               class="input-block-level form-control" value="{{ blk.test_correct[lang_code]  }}"
               placeholder="{_ Feedback if correct _} ({{ lang_code }})">
        <label class="control-label">{_ Feedback if correct _} ({{ lang_code }})</label>
    </div>

    <div class="form-group label-floating">
        <input type="text" id="block-{{name}}-test_wrong--{{ lang_code }}" name="blocks[].test_wrong${{ lang_code }}" 
               class="input-block-level form-control" value="{{ blk.test_wrong[lang_code]  }}"
               placeholder="{_ Feedback if wrong _} ({{ lang_code }})">
        <label class="control-label">{_ Feedback if wrong _} ({{ lang_code }})</label>
    </div>
</div>
