<div class="test-controls" {% if not blk.is_test %}style="display:none"{% endif %}>
    <div class="row">
        <div class="form-group col-md-6">
            <label class="control-label">{_ Feedback if correct _}</label>
            <input type="text" id="block-{{name}}-test_correct{{ lang_code_for_id }}" name="block-{{name}}-test_correct{{ lang_code_with_dollar }}" 
                   class="input-block-level form-control" value="{{ blk.test_correct[lang_code]  }}"
                   placeholder="" />
        </div>

        <div class="form-group col-md-6">
            <label class="control-label">{_ Feedback if wrong _}</label>
            <input type="text" id="block-{{name}}-test_wrong{{ lang_code_for_id }}" name="block-{{name}}-test_wrong{{ lang_code_with_dollar }}" 
                   class="input-block-level form-control" value="{{ blk.test_wrong[lang_code]  }}"
                   placeholder="" />
        </div>
    </div>
</div>
