You may consider getting your copy of OpenSSL <http://www.openssl.org> and generate your own certificate
and private key by issuing a command like:

openssl req -new -x509 -newkey rsa:1024 -days 365 -keyout server.key -out server.crt
