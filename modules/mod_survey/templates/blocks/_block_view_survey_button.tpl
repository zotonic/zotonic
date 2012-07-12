<div class="control-group survey-button">
    <div class="help-block">
        {{ blk.body }}
    </div>
    <button class="btn {{ blk.style }}" id="{{ #id }}" name="{{ blk.name }}" type="submit">{{ blk.prompt }}</button>
    <input type="hidden" value="{{ blk.name }}" name="survey$button" />
</div>
