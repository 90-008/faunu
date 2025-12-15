{
  inputs.parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.naked-shell.url = "github:90-008/mk-naked-shell";
  inputs.nci.url = "github:90-008/nix-cargo-integration";
  inputs.nci.inputs.nixpkgs.follows = "nixpkgs";

  outputs = inp:
    inp.parts.lib.mkFlake {inputs = inp;} {
      systems = ["x86_64-linux"];
      imports = [
        inp.naked-shell.flakeModule
        # inp.nci.flakeModule
      ];
      perSystem = {
        config,
        pkgs,
        ...
      }: {
        devShells.default = pkgs.mkShell {
          name = "faunu-devshell";
          packages = with pkgs; [
            nodejs-slim_latest deno
            nodePackages.svelte-language-server
            nodePackages.typescript-language-server
            rustc rust-analyzer cargo wasm-pack wasm-bindgen-cli lld rustfmt binaryen
          ];
          shellHook = ''
            export PATH="$PATH:$PWD/node_modules/.bin"
          '';
        };
        packages.faunu-wasm = pkgs.callPackage ./nix/wasm.nix {};
        packages.faunu-modules = pkgs.callPackage ./nix/modules.nix {};
        packages.faunu = pkgs.callPackage ./nix {
          inherit (config.packages) faunu-modules faunu-wasm;
        };
        packages.default = config.packages.faunu;
    };
  };
}
