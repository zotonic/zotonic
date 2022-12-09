let
  pkgs = import ./nix/nixpkgs.nix;
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    bashInteractive
    erlangR22
    rebar3
    erlfmt
    imagemagick
    ffmpeg
    postgresql
    gettext
    libiconv
    (if stdenv.isDarwin then fswatch else inotify-tools)
  ];
}
