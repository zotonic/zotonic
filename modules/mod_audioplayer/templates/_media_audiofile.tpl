<div id="jplayer{{ id }}"></div>
<div id="jloading{{ id }}"><img src="/lib/images/spinner.gif" alt="{_ Loading audio file _}" /></div>
<div id="jinterface{{ id }}" style="display: none">
 <a href="#" class="jp-play"><img src="/lib/images/audiofile_play.png" alt="{_ Play _}" /></a>
 <a href="#" class="jp-pause"><img src="/lib/images/audiofile_pause.png" alt="{_ Pause _}" /></a>
</div>

{% call resource_playaudiofile with id %}
