{
  description = "monoid-action-t";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    with builtins;
    with nixpkgs.lib;
    let
      inherit (nixpkgs) lib;
      pname = "monoid-action-t";
      path = ./.;

      # Always keep in sync with the tested-with section in the cabal file
      supportedGhcs = [
        # Not supported in nixpkgs anymore
        # "ghc86"
        # "ghc88"

        "ghc810"
        "ghc90"
        "ghc92"
        "ghc94"
        "ghc96"
        "ghc98"
        "ghc910"
        # "ghc912" # Uncomment as soon as nixpkgs is more advanced
      ];

      haskellPackagesFor = pkgs: genAttrs supportedGhcs (ghc: pkgs.haskell.packages.${ghc})
        // { default = pkgs.haskellPackages; };

      hoverlay = pkgs: hfinal: hprev: with pkgs.haskell.lib;
        {
          ${pname} = hfinal.callCabal2nix pname path { };
        };

      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = composeManyExtensions [
            (hoverlay prev)
            prev.haskell.packageOverrides
          ];
        };
      };

    in
    flake-utils.lib.eachDefaultSystem
      (system:

        let
          pkgs = nixpkgs.legacyPackages.${system}.extend overlay;
          forEachGHC = mapAttrs (_ghcVersion: haskellPackages: haskellPackages.${pname}) (haskellPackagesFor pkgs);
          allGHCs = pkgs.linkFarm "${pname}-all-ghcs" forEachGHC;
        in
        {
          packages = forEachGHC // {
            default = allGHCs;
          };

          devShells = mapAttrs
            (ghcVersion: haskellPackages: haskellPackages.shellFor {
              packages = hps: [ hps.${pname} ];
              nativeBuildInputs = (with haskellPackages;
                lib.optional (versionAtLeast haskellPackages.ghc.version "9.2") haskell-language-server)
              ++ (with pkgs;
                [ cabal-install ]
              )
              ;
            })
            (haskellPackagesFor pkgs);

          formatter = pkgs.nixpkgs-fmt;
        }) // {
      inherit supportedGhcs;
      overlays.default = overlay;
    };
}
