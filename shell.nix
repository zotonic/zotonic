let
  pkgs = import ./nix/nixpkgs.nix;
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    erlangR22
    fswatch
    imagemagick
    ffmpeg
    postgresql
    gettext
  ];
}
