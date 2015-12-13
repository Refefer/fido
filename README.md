fido
=====

A simple, fast remote file cache.

Install
-------

It's as simple as `stack install`

To Use
------

Using fido is very simple.  Simply specify the serving port and a list of download zones:

    fido -p 3456 '/data/images/zone1=>zone1' '/data/images/zone2=>zone2'

and then hit the desired endpoint:
    
    curl 'http://localhost:3456/zone1/https://www.google.com/images/branding/googlelogo/2x/googlelogo_color_272x92dp.png > google-logo.png'

or
    curl 'http://localhost:3456/zone2/https://www.google.com/images/branding/googlelogo/2x/googlelogo_color_272x92dp.png > google-logo.png'

Files will be downloaded to the appropriate cache.
