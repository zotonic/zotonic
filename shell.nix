{ pkgs ? import ./nix/nixpkgs.nix }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    bashInteractive
    erlang_26
    rebar3
    erlfmt
    imagemagick
    ffmpeg
    postgresql
    gettext
    libiconv
    (if stdenv.isDarwin then fswatch else inotify-tools)
    python
    python310Packages.pip
  ] ++ lib.optional (!stdenv.hostPlatform.isDarwin) libcap;
}
