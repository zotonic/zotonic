{% lib "js/modules/z.cropcenter.js" %}

<style>
    #rsc-image {
    position: relative;
    }
    div.cropguide {
    position: absolute;
    }
    div.cropguide.horiz {
    border-top: 1px dotted rgba(255,255,255,0.7);
    border-bottom: 1px dotted rgba(0,0,0,0.7);
    height: 0;
    left: 0; right: 0;
    }
    div.cropguide.vert {
    border-left: 1px dotted rgba(255,255,255,0.7);
    border-right: 1px dotted rgba(0,0,0,0.7);
    width: 0;
    top: 0; bottom: 0;
    }
</style>

<input type="hidden" name="crop_center" id="crop_center" value="{{ r.crop_center }}" />
{% button id="remove-crop-center" class="btn" icon="icon-remove" text=_"Remove crop center" %}
<span id="crop-center-message" class="alert">{_ Click the image to set the cropping center. _}</span>
{% javascript %}
    $("#rsc-image").doCropCenterEditor($("#crop_center"), $("#remove-crop-center"), $("#crop-center-message"));
{% endjavascript %}
