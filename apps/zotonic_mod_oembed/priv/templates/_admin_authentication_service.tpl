{% wire id="admin_oembed" type="submit" postback="admin_oembed" delegate=`mod_oembed` %}
<form name="admin_oembed" id="admin_oembed" class="form-horizontal" method="POST" action="postback">
    <div class="row">
        <div class="col-md-6">
            <div class="widget">
                <h3 class="widget-header"><span class="fa fa-globe"></span> Embedly</h3>
                <div class="widget-content">
                    <p class="help-block">
                        {_ API Key can be found on _} <a href="https://app.embed.ly/" title="embed.ly" target="_blank">{_ your Embedly app dashboard _}</a>
                    </p>

                    <div class="form-group row">
                        <label class="control-label col-md-3" for="embedly_key">{_ API Key _}</label>
                        <div class="col-md-9">
                            <input type="text" id="embedly_key" name="embedly_key" value="{{ m.config.mod_oembed.embedly_key.value|escape }}" class="form-control" />
                        </div>
                    </div>

                    <div class="form-group row">
                        <div class="col-md-9  col-md-offset-3">
                            <button class="btn btn-primary" type="submit">{_ Save Embedly Key _}</button>
                        </div>
                    </div>

                </div>
            </div>
        </div>
    </div>
</form>
