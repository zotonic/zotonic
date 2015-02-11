<div class="modal-body">

    <div class="form-group row">
        <label class="control-label" for="ffmpeg">{_ ffmpeg video conversion command line _}</label>
        <div>
            <input type="text" id="ffmpeg" name="api_key" value="{{ m.config.mod_video.ffmpeg_cmdline.value|escape }}" class="do_autofocus col-lg-4 col-md-4 form-control" />
            {% wire id="ffmpeg" type="blur" action={config_toggle module="mod_video" key="ffmpeg_cmdline" } %}
        </div>
    </div>
    
    <div class="form-group row">
        <p class="info-block">
            {_ Please specify the commandline to the <tt>ffmpeg</tt> (or <tt>avconv</tt>) command. Put the marker <b>~s</b> on the place where the input filename should go. The output filename is appended automatically to the commandline before execution, as is a correction for fixing the video's orientation. Example value: _}
        </p>

        <p><small><pre>ffmpeg -i ~s -vcodec libx264 -loglevel fatal -f mp4 -strict -2 -y -movflags +faststart -preset medium -metadata:s:v:0 rotate=0</pre></small></p>
        
    </div>

    <div class="form-group row">
        <label class="control-label" for="ffmpegpreview">{_ ffmpeg video preview command line _}</label>
        <div>
            <input type="text" id="ffmpegpreview" name="api_key" value="{{ m.config.mod_video.ffmpeg_preview_cmdline.value|escape }}" class="col-lg-4 col-md-4 form-control" />
            {% wire id="ffmpegpreview" type="blur" action={config_toggle module="mod_video" key="ffmpeg_preview_cmdline" } %}
        </div>
    </div>
    
    <div class="form-group row">
        <p class="info-block">
            {_ Please specify the commandline to the <tt>ffmpeg</tt> (or <tt>avconv</tt>) command for generating the video preview image (poster frame). Put the marker <b>~s</b> on the place where the input filename should go. The output filename is appended automatically to the commandline before execution, as is a correction for fixing the video's orientation. Example value: _}
        </p>

        <p><small><pre>ffmpeg -i ~s -vcodec png -vframes 1 -an -f rawvideo -loglevel error -y</pre></small></p>
        
    </div>
    
    <div class="form-group row">
        <label class="control-label" for="ffprobe">{_ ffprobe command line _}</label>
        <div>
            <input type="text" id="ffprobe" name="api_key" value="{{ m.config.mod_video.ffprobe_cmdline.value|escape }}" class="col-lg-4 col-md-4 form-control" />
            {% wire id="ffprobe" type="blur" action={config_toggle module="mod_video" key="ffprobe_cmdline" } %}
        </div>
    </div>
    
    <div class="form-group row">
        <p class="info-block">
            {_ Please specify the commandline to the <tt>ffprobe</tt> (or <tt>avprobe</tt>) command. The input filename is appended automatically to the commandline before execution. Example value: _}
        </p>

        <p><small><pre>ffprobe -loglevel quiet -show_format -show_streams -print_format json</pre></small></p>        
    </div>
    
    <div class="form-group row">
        {% wire id="tpldbg" 
            action={config_toggle module="mod_video" key="logging"}
            action={admin_tasks task='flush'} 
        %}
        <label class="checkbox-inline">
            <input type="checkbox" id="tpldbg" value="1" {% if m.config.mod_video.logging.value %}checked="checked"{% endif %} />
            {_ Log every transcoding command in the Zotonic console _}
        </label>
    </div>
    

</div>

<div class="modal-footer">
    {% button class="btn btn-default" text=_"Close" action={dialog_close} tag="a" %}
</div>

