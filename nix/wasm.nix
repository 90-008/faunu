{
  rustPlatform,
  lib,
  wasm-pack,
  binaryen,
  wasm-bindgen-cli,
  lld,
  stdenv,
  ...
}:
let
  # First derivation: wasm-pack build
  wasmBuild = rustPlatform.buildRustPackage {
    pname = "faunu-wasm-build";
    version = "main";

    src = lib.fileset.toSource {
      root = ../.;
      fileset = lib.fileset.unions [
        ../Cargo.toml
        ../Cargo.lock
        ../src
        ../.cargo
      ];
    };

    cargoLock = {
      lockFile = ../Cargo.lock;
      outputHashes = {
        "vfs-0.12.1" = "sha256-d249sIYhICdqqb7uoTRyhXAZTCF5zgjfItM4DE7b/gQ=";
        "jacquard-0.9.4" = "sha256-TEu4coueWzzkmFCkGb610Xrly7n8LUGMa5tdde/OElg=";
      };
    };

    nativeBuildInputs = [wasm-pack wasm-bindgen-cli lld];

    phases = ["unpackPhase" "buildPhase"];

    buildPhase = ''
      mkdir -p $out
      HOME=$TMPDIR wasm-pack build --target web --release --no-opt -d $out
    '';
  };

  # Second derivation: wasm-opt
  wasmOpt = stdenv.mkDerivation {
    pname = "faunu-wasm";
    version = "main";

    src = wasmBuild;

    nativeBuildInputs = [binaryen];

    phases = ["unpackPhase" "buildPhase"];

    buildPhase = ''
      mkdir -p $out
      cp -r --no-preserve=ownership,mode ${wasmBuild}/* $out/
      wasm-opt $out/faunu_bg.wasm -O4 -all -o $out/faunu_bg.wasm
    '';
  };
in
  wasmOpt
