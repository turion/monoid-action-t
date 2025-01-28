{
  description = "monoid-action-t";

  nixConfig = {
    extra-substituters = [
      "https://monoid-action-t.cachix.org"
    ];
    extra-trusted-public-keys = [
      "monoid-action-t.cachix.org-1:qvZhVk91mkIWKyJoppTYKSlNrMXPa3befie8V3L0BAk="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    with builtins;
    with nixpkgs.lib;
    let
      inherit (nixpkgs) lib;
      projectName = "monoid-action-t";
      localPackages = {
        monoid-action-t = ./monoid-action-t;
        monoid-action-t-containers = ./monoid-action-t-containers;
        monoid-action-t-fused-effects = ./monoid-action-t-fused-effects;
        monoid-action-t-lens = ./monoid-action-t-lens;
      };

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
        (mapAttrs (pname: path: hfinal.callCabal2nix pname path { }) localPackages)
        // optionalAttrs (versionOlder hprev.ghc.version "9.6") {
          # exceptions = hfinal.exceptions_0_10_9;
          # template-haskell = hfinal.template-haskell_2_23_0_0;
          # filepath = dontBenchmark (dontCheck hfinal.filepath_1_5_4_0);
          # mtl = hfinal.mtl_2_3_1;
        };

      haskellPackagesExtended = pkgs: mapAttrs
        (ghcVersion: haskellPackages: haskellPackages.override (_: {
          overrides = (hoverlay pkgs);
        }))
        (haskellPackagesFor pkgs);

      localPackagesFor = haskellPackages: mapAttrs (pname: _path: haskellPackages.${pname}) localPackages;
      allLocalPackagesFor = pkgs: ghcVersion: haskellPackages:
        pkgs.linkFarm "${projectName}-all-for-${ghcVersion}"
          (localPackagesFor haskellPackages);
    in
    flake-utils.lib.eachDefaultSystem
      (system:

        let
          pkgs = nixpkgs.legacyPackages.${system};
          forEachGHC = mapAttrs (allLocalPackagesFor pkgs) (haskellPackagesExtended pkgs);
          allGHCs = pkgs.linkFarm "${projectName}-all-ghcs" forEachGHC;
        in
        {
          # "packages" doesn't allow nested sets
          legacyPackages = mapAttrs
            (ghcVersion: haskellPackages: localPackagesFor haskellPackages // {
              "${projectName}-all" = allLocalPackagesFor pkgs ghcVersion haskellPackages;
            })
            (haskellPackagesExtended pkgs) // {
            "${projectName}-all" = forEachGHC;
          };

          packages = {
            default = allGHCs;
          };

          devShells = mapAttrs
            (ghcVersion: haskellPackages: haskellPackages.shellFor {
              packages = hps: attrValues (localPackagesFor haskellPackages);
              nativeBuildInputs = (
                lib.optional (versionAtLeast haskellPackages.ghc.version "9.2")
                  haskellPackages.haskell-language-server)
              ++ (with pkgs;
                [ cabal-install ]
              )
              ;
            })
            (haskellPackagesExtended pkgs);

          formatter = pkgs.nixpkgs-fmt;
        }) // {
      inherit supportedGhcs;
      inherit hoverlay;
    };
}
