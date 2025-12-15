{
  lib,
  stdenv,
  deno,
  nodejs,
  makeBinaryWrapper,
  callPackage,
  faunu-modules ? callPackage ./modules.nix {},
  faunu-wasm ? callPackage ./wasm.nix {},
  PUBLIC_BASE_URL ? "http://localhost:5173",
}:
stdenv.mkDerivation {
  name = "faunu";

  src = lib.fileset.toSource {
    root = ../.;
    fileset = lib.fileset.unions [
      ../.npmrc
      ../deno.lock
      ../deno.json
      ../www
    ];
  };

  nativeBuildInputs = [makeBinaryWrapper];
  buildInputs = [deno];

  inherit PUBLIC_BASE_URL;

  dontCheck = true;

  configurePhase = ''
    runHook preConfigure
    cp -R --no-preserve=ownership,mode ${faunu-modules} node_modules
    find node_modules -type d -exec chmod 755 {} \;
    find -L node_modules -type f -path '*/bin/*' -exec chmod +x {} \;
    substituteInPlace node_modules/.bin/vite \
      --replace-fail "/usr/bin/env node" "${nodejs}/bin/node"
    ln -sf ${faunu-wasm} pkg
    runHook postConfigure
  '';
  buildPhase = ''
    runHook preBuild
    HOME=$TMPDIR deno task --cwd www --frozen build
    runHook postBuild
  '';
  installPhase = ''
    runHook preInstall

    mkdir -p $out
    cp -R ./www/dist/* $out

    runHook postInstall
  '';
}
