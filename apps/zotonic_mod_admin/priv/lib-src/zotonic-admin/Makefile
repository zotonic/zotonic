MATERIAL = ../../../../zotonic_mod_artwork/priv/lib-src/material-design/less
FA4 = ../../../../zotonic_mod_artwork/priv/lib-src/font-awesome-4/less

../../lib/css/zotonic-admin.css: less/* $(MATERIAL)/* $(FA4)/*
	lessc --include-path="$(FA4):$(MATERIAL)" \
	    less/zotonic-admin.less \
	    ../../lib/css/zotonic-admin.css
