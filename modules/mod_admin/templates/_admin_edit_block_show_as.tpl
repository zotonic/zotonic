<fieldset class="form-vertical">
    <label for="block-{{name}}-style">{_ Show as _}</label>
    <select id="block-{{name}}-style" name="block-{{name}}-style">
         <option value="inline">{_ Inline text block _}</option>
         <option value="info" {% if blk.style == "info" %}selected="selected"{% endif %}>{_ Info link with modal popup _}</option>
         <option value="quote" {% if blk.style == "quote" %}selected="selected"{% endif %}>{_ Block quote _}</option>
         <option value="aside" {% if blk.style == "aside" %}selected="selected"{% endif %}>{_ Aside _}</option>
    </select>
</fieldset>
