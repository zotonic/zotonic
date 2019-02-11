Clamd Notes
===========

Protocol:

    https://linux.die.net/man/8/clamd

Install clamd (Unbuntu):

    sudo apt-get install clamav-daemon

    (..)

     * Clamav signatures not found in /var/lib/clamav
     * Please retrieve them using freshclam
     * Then run 'invoke-rc.d clamav-daemon start'


Config:

    /etc/clamav/clamd.conf

    #To reconfigure clamd run #dpkg-reconfigure clamav-daemon


Scan a file containing a virus:

    marc@nuc:~$ telnet localhost 3310
    SCAN /home/marc/virus/eicar_com.zip
    /home/marc/virus/eicar_com.zip: Eicar-Test-Signature FOUND
    Connection closed by foreign host.

Scan an ok file:

    marc@nuc:~$ telnet localhost 3310
    SCAN /home/marc/PDf-THAT_WILL_NOT_UPLOAD.pdf
    /home/marc/PDf-THAT_WILL_NOT_UPLOAD.pdf: OK
    Connection closed by foreign host.

