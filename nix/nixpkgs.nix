import
  (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/219b2fc946a59851cdfc59efdd11129e1cee2b44.zip"; # Most recent 22.11 commit as of 5 Dec 2022 16:18
    sha256 = "sha256:05sgy7bdbqyyin07vzbx8fnc4mjw5780qf0r1ia8188ss2vxw4yr";
  })
{ }
