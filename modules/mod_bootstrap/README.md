# Upgrading

In order to update the bootstrap library for zotonic you need to use the script
found in the `priv` directory of this module. It will download the zip from the
bootstrap site and change some of the embedded paths in the css and map files.

Goto the bootstrap website, and copy the download link of the release you want
to upgrade to. Use this link as argument to the `update-bootstrap` script.

Example:

```
cd priv
$ ./update-bootstrap https://github.com/twbs/bootstrap/releases/download/v3.4.1/bootstrap-3.4.1-dist.zip
--2019-03-13 11:48:42--  https://github.com/twbs/bootstrap/releases/download/v3.4.1/bootstrap-3.4.1-dist.zip
Resolving github.com (github.com)... 192.30.253.113, 192.30.253.112
Connecting to github.com (github.com)|192.30.253.113|:443... connected.
HTTP request sent, awaiting response... 302 Found
Location: https://github-production-release-asset-2e65be.s3.amazonaws.com/2126244/d2e19d80-2fb8-11e9-85b1-ef585ab03b84?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIAIWNJYAX4CSVEH53A%2F20190313%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20190313T104843Z&X-Amz-Expires=300&X-Amz-Signature=b84041d6854caea48809ee072270a8ee394e0f6481380e06437760ea8b138b50&X-Amz-SignedHeaders=host&actor_id=0&response-content-disposition=attachment%3B%20filename%3Dbootstrap-3.4.1-dist.zip&response-content-type=application%2Foctet-stream [following]
--2019-03-13 11:48:43--  https://github-production-release-asset-2e65be.s3.amazonaws.com/2126244/d2e19d80-2fb8-11e9-85b1-ef585ab03b84?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIAIWNJYAX4CSVEH53A%2F20190313%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20190313T104843Z&X-Amz-Expires=300&X-Amz-Signature=b84041d6854caea48809ee072270a8ee394e0f6481380e06437760ea8b138b50&X-Amz-SignedHeaders=host&actor_id=0&response-content-disposition=attachment%3B%20filename%3Dbootstrap-3.4.1-dist.zip&response-content-type=application%2Foctet-stream
Resolving github-production-release-asset-2e65be.s3.amazonaws.com (github-production-release-asset-2e65be.s3.amazonaws.com)... 52.216.129.147
Connecting to github-production-release-asset-2e65be.s3.amazonaws.com (github-production-release-asset-2e65be.s3.amazonaws.com)|52.216.129.147|:443... connected.
HTTP request sent, awaiting response... 200 OK
Length: 371392 (363K) [application/octet-stream]
Saving to: ‘bootstrap.zip’

bootstrap.zip                      100%[=============================================================>] 362.69K   741KB/s    in 0.5s    

2019-03-13 11:48:44 (741 KB/s) - ‘bootstrap.zip’ saved [371392/371392]

Archive:  bootstrap.zip
   creating: ./bootstrap-3.4.1-dist/
   creating: ./bootstrap-3.4.1-dist/js/
  inflating: ./bootstrap-3.4.1-dist/js/bootstrap.js  
  inflating: ./bootstrap-3.4.1-dist/js/bootstrap.min.js  
  inflating: ./bootstrap-3.4.1-dist/js/npm.js  
   creating: ./bootstrap-3.4.1-dist/css/
  inflating: ./bootstrap-3.4.1-dist/css/bootstrap-theme.min.css  
  inflating: ./bootstrap-3.4.1-dist/css/bootstrap.css.map  
  inflating: ./bootstrap-3.4.1-dist/css/bootstrap-theme.min.css.map  
  inflating: ./bootstrap-3.4.1-dist/css/bootstrap-theme.css.map  
  inflating: ./bootstrap-3.4.1-dist/css/bootstrap.css  
  inflating: ./bootstrap-3.4.1-dist/css/bootstrap.min.css  
  inflating: ./bootstrap-3.4.1-dist/css/bootstrap-theme.css  
  inflating: ./bootstrap-3.4.1-dist/css/bootstrap.min.css.map  
   creating: ./bootstrap-3.4.1-dist/fonts/
  inflating: ./bootstrap-3.4.1-dist/fonts/glyphicons-halflings-regular.eot  
  inflating: ./bootstrap-3.4.1-dist/fonts/glyphicons-halflings-regular.woff2  
  inflating: ./bootstrap-3.4.1-dist/fonts/glyphicons-halflings-regular.svg  
  inflating: ./bootstrap-3.4.1-dist/fonts/glyphicons-halflings-regular.ttf  
  inflating: ./bootstrap-3.4.1-dist/fonts/glyphicons-halflings-regular.woff
```

After this step, all the necessary files are placed in the directories where
zotonic expects them to be. 

After this step, a normal commit will put the new boostrap code into our 
repository.

