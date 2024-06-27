{
  description = "Unison";

  nixConfig = {
    extra-substituters = ["https://unison.cachix.org"];
    extra-trusted-public-keys = ["unison.cachix.org-1:i1DUFkisRPVOyLp/vblDsbsObmyCviq/zs6eRuzth3k="];
  };

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs-haskellNix.follows = "haskellNix/nixpkgs-unstable";
    nixpkgs-release.url = "github:NixOS/nixpkgs/release-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    haskellNix,
    nixpkgs-haskellNix,
    nixpkgs-release,
    flake-utils,
  }:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ]
    (system: let
      versions = {
        ormolu = "0.5.2.0";
        hls = "2.4.0.0";
        stack = "2.13.1";
        hpack = "0.35.2";
      };
      overlays = [
        haskellNix.overlay
        (import ./nix/unison-overlay.nix)
      ];
      pkgs = import nixpkgs-haskellNix {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      unison-project = import ./nix/unison-project.nix {
        inherit (nixpkgs-haskellNix) lib;
        inherit (pkgs) haskell-nix;
      };
      haskell-nix-flake = import ./nix/haskell-nix-flake.nix {
        inherit pkgs unison-project versions;
        inherit (tool-pkgs) stack hpack;
      };
      release-pkgs = import nixpkgs-release {
        inherit system;
        overlays = [(import ./nix/unison-overlay.nix)];
      };
      tool-pkgs = {
        stack = release-pkgs.unison-stack;
        unwrapped-stack = release-pkgs.stack;
        hpack = release-pkgs.hpack;
      };
      renameAttrs = fn:
        nixpkgs-haskellNix.lib.mapAttrs' (name: value: {
          inherit value;
          name = fn name;
        });
    in
      assert tool-pkgs.unwrapped-stack.version == versions.stack;
      assert tool-pkgs.hpack.version == versions.hpack; {
        packages =
          renameAttrs (name: "component-${name}") haskell-nix-flake.packages
          // renameAttrs (name: "docker-${name}") (import ./nix/docker.nix {
            inherit pkgs;
            haskell-nix = haskell-nix-flake.packages;
          })
          // {
            default = haskell-nix-flake.defaultPackage;
            build-tools = pkgs.symlinkJoin {
              name = "build-tools";
              paths = self.devShells."${system}".only-tools.buildInputs;
            };
            all = pkgs.symlinkJoin {
              name = "all";
              paths = let
                all-other-packages = builtins.attrValues (builtins.removeAttrs self.packages."${system}" ["all" "build-tools"]);
                ## FIXME: Including these inputs currently results in massing GHC builds.
                devshell-inputs = [];
                  # builtins.concatMap
                  # (devShell: devShell.buildInputs ++ devShell.nativeBuildInputs)
                  # (builtins.attrValues self.devShells."${system}");
              in
                all-other-packages ++ devshell-inputs;
            };
          };

        apps =
          renameAttrs (name: "component-${name}") haskell-nix-flake.apps
          // {default = self.apps."${system}"."component-unison-cli-main:exe:unison";};

        devShells =
          renameAttrs (name: "cabal-${name}") haskell-nix-flake.devShells
          // {default = self.devShells."${system}".cabal-only-tools;};

        checks = renameAttrs (name: "component-${name}") haskell-nix-flake.checks;

        formatter = pkgs.alejandra;
      });
}
