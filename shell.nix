{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [
      nix-prefetch-git
      figlet

      swiProlog
    ];

    shellHook = ''
      figlet "Logic programming"
    '';
  }
