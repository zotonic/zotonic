<p>
    {_ Upload a CSV or XLSX file from your computer. _}
    {_ Depending on the filename the data can be imported as pages or as special data. _}
</p>

{% wire id=#form type="submit" delegate="mod_import_csv" postback={csv_upload} %}
<form id="{{ #form }}" method="POST" action="postback">

	<div class="form-group">
		<label class="control-label" for="upload_file">{_ Select file _}</label>
        <input class="form-control" type="file" id="upload_file" name="upload_file">
        {% validate id="upload_file" type={presence} %}
    </div>

	<div class="form-group">
		<label class="checkbox">
            <input type="checkbox" name="reset" id="reset" value="true">
            {_ Import any previously deleted items again _}
        </label>
    </div>

    <p class="help-block">
        {_ The import will be done in the background after the file has been uploaded to the server. _}
        {% trans "Progress will be reported in the <a href=\"{url}\">admin log</a> and with notifications in the admin."
                 url=m.dispatch.url_for.admin_log
        %}
    </p>

    <div class="modal-footer">
        {% button class="btn btn-default" action={dialog_close} text="Cancel" tag="a" %}
        <button class="btn btn-primary" type="submit">{_ Start import _}</button>
    </div>
</form>
