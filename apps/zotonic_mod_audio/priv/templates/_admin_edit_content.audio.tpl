{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
    {_ Media tags _}
{% endblock %}

{% block widget_id %}content-audio-tags{% endblock %}

{% block widget_content %}
    <div class="row">
        <div class="col-lg-6 col-md-6">
            <div class="form-group label-floating">
                <input class="form-control" id="audio-album" type="text" name="album" value="{{ id.album }}" placeholder="{_ Album _}">
                <label class="control-label" for="audio-album">{_ Album _}</label>
            </div>
            <div class="form-group label-floating">
                <input class="form-control" id="audio-artist" type="text" name="artist" value="{{ id.artist }}" placeholder="{_ Artist _}">
                <label class="control-label" for="audio-artist">{_ Artist _}</label>
            </div>
            <div class="form-group label-floating">
                <input class="form-control" id="audio-album_artist" type="text" name="album_artist" value="{{ id.album_artist }}" placeholder="{_ Album artist _}">
                <label class="control-label" for="audio-album_artist">{_ Album artist _}</label>
            </div>
            <div class="form-group">
                <label class="checkbox">
                    <input id="audio-is_compilation" type="checkbox" name="is_compilation" value="1" {% if id.is_compilation %}checked{% endif %}>
                    {_ Album is a compilation of songs by various artists _}
                </label>
            </div>
            <div class="form-group label-floating">
                <input class="form-control" id="audio-composer" type="text" name="composer" value="{{ id.composer }}" placeholder="{_ Composer _}">
                <label class="control-label" for="audio-composer">{_ Composer _}</label>
            </div>
        </div>

        <div class="col-lg-6 col-md-6">
            <div class="form-group label-floating">
                <input class="form-control" id="audio-genre" type="text" name="genre" value="{{ id.genre }}" placeholder="{_ Genre _}">
                <label class="control-label" for="audio-genre">{_ Genre _}</label>
            </div>
            <div class="form-group label-floating">
                <input class="form-control" id="audio-track" type="text" name="track" value="{{ id.track }}" placeholder="{_ Track _}">
                <label class="control-label" for="audio-track">{_ Track _}</label>
            </div>
            <div class="form-group label-floating">
                <input class="form-control" id="audio-copyright" type="text" name="copyright" value="{{ id.copyright }}" placeholder="{_ Copyright _}">
                <label class="control-label" for="audio-copyright">{_ Copyright _}</label>
            </div>
        </div>
    </div>
</fieldset>
{% endblock %}

