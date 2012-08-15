<p>
    {_ Upload a wordpress WXR file from your computer. _}
</p>

{% wire id=#form type="submit" delegate="mod_import_wordpress" postback={wxr_upload} %}
<form id="{{ #form }}" method="POST" action="postback" class="form-horizontal">

    <div class="control-group">
        <label class="control-label" for="upload_file">{_ WXR file _}</label>
        <div class="controls">
            <input type="file" id="upload_file" name="upload_file" />
            {% validate id="upload_file" type={presence} %}
        </div>
    </div>

    <div class="control-group">
        <label class="control-label">&nbsp;</label>
        <div class="controls">
            <label class="inline checkbox"><input type="checkbox" name="reset" id="reset" value="true" />
            {_ Import previously deleted items again _}
            </label>
        </div>
    </div>

    <div class="modal-footer">
        {% button class="btn" action={dialog_close} text="Cancel" tag="a" %}
        <button class="btn btn-primary" type="submit">{_ Start import _}</button>
    </div>
</form>

