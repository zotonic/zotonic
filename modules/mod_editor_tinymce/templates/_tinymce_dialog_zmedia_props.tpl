<form id="zmedia-props-form" class="form">
     <div class="form-group row">
          <div class="col-md-6">
               <div class="form-group">
                    <img style="width: 100%" src="{% url admin_media_preview id=id %}" class="z-tinymce-media-left" />
               </div>
               <div class="form-group">
                    <label class="control-label">{_ Caption _}</label>
                    <div class="controls">
                         <textarea class="form-control input-block-level" name="caption" id="a-caption">{{ options.caption|escape }}</textarea>
                         <p class="help-block">
                              {_ Defaults to the summary of the media. Enter a single “-” to not display a caption. _}
                         </p>
                    </div>
               </div>
          </div>
          <div class="col-md-6">
               <div class="row">
                    <div class="col-md-6">
                         <div class="form-group">
                              <label class="control-label">{_ Alignment _}</label>
                              <div class="controls">
                                   <div class="radio">
                                        <label>
                                             <input type="radio" name="align" {% if options.align == 'block' %}checked{% endif %} value="block" id="a-block">
                                             {_ Between text _}
                                        </label>
                                   </div>
                                   <div class="radio">
                                        <label>
                                             <input type="radio" name="align" {% if options.align == 'left' %}checked{% endif %} value="left" id="a-left">
                                             {_ Aligned left _}
                                        </label>
                                   </div>
                                   <div class="radio">
                                        <label>
                                             <input type="radio" name="align" {% if options.align == 'right' %}checked{% endif %} value="right" id="a-right">
                                             {_ Aligned right _}
                                        </label>
                                   </div>
                              </div>
                         </div>
                         <div class="form-group">
                              <label class="control-label">{_ Crop _}</label>
                              <div class="controls">
                                   <div class="checkbox">
                                        <label>
                                             <input type="checkbox" name="crop" {% if options.crop %}checked{% endif %} value="crop" id="a-crop">
                                             {_ Crop image _}
                                        </label>
                                   </div>
                              </div>
                         </div>
                    </div>
                    <div class="col-md-6">
                         <div class="form-group">
                              <label class="control-label">{_ Size _}</label>
                              <div class="controls">
                                   <div class="radio">
                                        <label>
                                             <input type="radio" name="size" {% if options.size == 'small' %}checked{% endif %} value="small" id="a-small">
                                             {_ Small _}
                                        </label>
                                   </div>
                                   <div class="radio">
                                        <label>
                                             <input type="radio" name="size" {% if options.size == 'middle' %}checked{% endif %} value="middle" id="a-middle"> {_ Medium _}
                                        </label>
                                   </div>
                                   <div class="radio">
                                        <label>
                                             <input type="radio" name="size" {% if options.size == 'large' %}checked{% endif %} value="large" id="a-large">
                                             {_ Large _}
                                        </label>
                                   </div>
                              </div>
                         </div>
                    </div>
               </div>
               <div class="row">
                    <div class="col-md-12">
                         <div class="form-group">
                              <label class="control-label">{_ Link _}</label>
                              <div class="checkbox">
                                   <label>
                                        <input type="checkbox" name="link" {% if options.link %}checked{% endif %} value="link" id="a-link">
                                        {_ Link to media or url below _}
                                   </label>
                              </div>
                              <div class="checkbox">
                                   <input type="text" class="form-control input-block-level" name="link_url" id="a-link_url" placeholder="{_ Website. Leave empty for media link _}" value="{{ options.link_url|escape }}">
                              </div>
                         </div>
                    </div>
               </div>
          </div>
     </div>
     <div class="modal-footer">
          <button class="btn btn-primary" type="submit">{_ Save _}</button>
          <button class="btn btn-default" id="{{ #cancel }}">{_ Cancel _}</button>
     </div>
</form>

{% wire id=#cancel action={dialog_close} %}
