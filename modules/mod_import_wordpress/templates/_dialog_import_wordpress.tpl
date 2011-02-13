<p>
    {_ Upload a wordpress WXR file from your computer. _}
</p>


{% wire id=#form type="submit" delegate="mod_import_wordpress" postback={wxr_upload} %}
<form id="{{ #form }}" method="POST" action="postback">
    <div class="new-media-wrapper">

        <div class="form-item clearfix">
            <label for="upload_file">{_ WXR file _}</label>
            <input type="file" id="upload_file" name="upload_file" />
            {% validate id="upload_file" type={presence} %}
        </div>

        <div class="form-item clearfix">
	    <input type="checkbox" name="reset" id="reset" value="true" />
	    {_ Import previously deleted items again _}
        </div>

        <div class="form-item clearfix">
            <button type="submit">{_ Start import _}</button>
            {% button action={dialog_close} text="Cancel" %}
        </div>
    </div>
</form>

