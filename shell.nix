{ pkgs ? import ./nix/nixpkgs.nix }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    bashInteractive
    erlang_28
    rebar3
    erlfmt
    imagemagick
    ghostscript
    ffmpeg
    postgresql
    gettext
    libiconv
    (if stdenv.isDarwin then fswatch else inotify-tools)
    python3
    python314Packages.python
  ] ++ lib.optional (!stdenv.hostPlatform.isDarwin) libcap;
}
