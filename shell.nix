let
  nixpkgs-2003 = builtins.fetchTarball {
    name = "nixos-19.03";
    url = https://github.com/nixos/nixpkgs/archive/1c6bdbc766e975b8ccd9b52e08d932e564ece1d8.tar.gz;
    sha256 = "07n2gqk475jy34spgfpk9m85hl8k66s5rrgqjclgh810af49g2zj";
  };

  pkgs = import nixpkgs-2003 {} ; #/nixpkgs.nix;

  ghcInputs = ghc: [
    ghc.array
    ghc.bitwise
    ghc.comonad
    ghc.JuicyPixels
  ];

  shell = pkgs.stdenv.mkDerivation {
    name = "hascell-development-shell";
    buildInputs = [
      pkgs.cabal-install
      pkgs.nix
      pkgs.haskellPackages.hp2html
      pkgs.haskellPackages.hp2pretty
      (pkgs.haskellPackages.ghcWithPackages ghcInputs)
    ];
  };
in
  shell
