QR Code Encoder
===============

Reference used was ISO/IEC 18004, 1st Edition (2000)

This implementation is informed by my specific needs, i.e. to provide
two-factor authentication for mobile phones running Google Authenticator.

+ "Byte" mode only (don't need e.g. numeric mode or kanji mode).
+ Encode only (no detection/decode).
+ Basic supporting library functions provided (HOTP, PNG image functions) to allow full-cyle demo.

Demo
====

1. Download repo and compile with `erl -make`
2. Install Google Authenticator App on your mobile:
    + iPhone:  http://itunes.apple.com/us/app/google-authenticator/id388497605?mt=8
    + Android: https://market.android.com/details?id=com.google.android.apps.authenticator
3. Run demo: `qrcode_demo:run().`
4. Open the generated `qrcode.png` file
5. Scan the qrcode into the phone.
6. Ensure server clock is correct.
7. The value of `qrcode_demo:totp()` should show the same passcode as the phone.
8. Handle PINs/logins for the second part of the "two factor" according to your application design.

NOTE: This documentation is rather basic as this was open-sourced by specific request!


https://github.com/komone/qrcode
