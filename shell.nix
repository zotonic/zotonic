let
  pkgs = import ./nix/nixpkgs.nix;
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    erlangR22
    imagemagick
    ffmpeg
    postgresql
    gettext
    (if stdenv.isDarwin then fswatch else inotify-tools)
  ];
}
