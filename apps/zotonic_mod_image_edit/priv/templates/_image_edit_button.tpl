{% button class="btn btn-default pull-left"
          action={dialog_close}
          action={overlay_open id=id
                template="_overlay_image_edit.tpl"
                class="dark image-edit-overlay"
                level="top"
          }
          text=_"Image editor"
          title=_"Change the image rotation, crop and contrast."
%}
