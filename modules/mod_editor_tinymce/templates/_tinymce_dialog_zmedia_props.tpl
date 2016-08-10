<form id="zmedia-props-form" class="form">
     <div class="form-group row">
          <div class="col-md-6">
               <img style="width: 100%" src="{% url admin_media_preview id=id %}" class="z-tinymce-media-left" />
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
                                             <input type="checkbox" name="crop" {% if options.crop == 'crop' %}checked{% endif %} value="crop" id="a-crop">
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
                                             <input type="radio" name="size" {% if options.size == 'middle' %}checked{% endif %} value="middle" id="a-middle"> {_ Middle _}
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
                         <div class="form-group">
                              <label class="control-label">{_ Link _}</label>
                              <div class="controls">
                                   <div class="checkbox">
                                        <label>
                                             <input type="checkbox" name="link" {% if options.link %}checked{% endif %} value="link" id="a-link">
                                             {_ Make link _}
                                        </label>
                                   </div>
                              </div>
                         </div>
                    </div>
               </div>
               <div class="row">
                    <div class="col-md-12">
                         <div class="form-group">
                              <label class="control-label">{_ Caption _}</label>
                              <div class="controls">
                                   <textarea class="form-control" name="caption" id="a-caption">{{ options.caption|escape }}</textarea>
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
</form>'

{% wire id=#cancel action={dialog_close} %}
