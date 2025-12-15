{
  lib,
  stdenv,
  deno,
}:
stdenv.mkDerivation {
  name = "faunu-modules";

  src = lib.fileset.toSource {
    root = ../.;
    fileset = lib.fileset.unions [
      ../.npmrc
      ../deno.lock
      ../deno.json
      ../www
    ];
  };

  outputHash = "sha256-Vl0zc9YI7zNIqBB3Bj2B+lqxT4jWCRihGClyW0FHBmM=";
  outputHashAlgo = "sha256";
  outputHashMode = "recursive";

  nativeBuildInputs = [deno];

  dontConfigure = true;
  dontCheck = true;
  dontFixup = true;
  dontPatchShebangs = true;

  postUnpack = ''

  '';
  buildPhase = ''
    HOME=$TMPDIR deno install --allow-scripts=npm:@swc/core --frozen --seed 8008135
  '';
  installPhase = ''
    cp -R node_modules $out
    ls -la $out
  '';
}
