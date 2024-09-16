# generated via cabal2nix
{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, cmdargs, crypton, directory
      , filepath, hspec, lib, memory, process, text, unix, xdg-basedir
      }:
      mkDerivation {
        pname = "hlesspass";
        version = "0.2.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base bytestring crypton memory text ];
        executableHaskellDepends = [
          base bytestring cmdargs directory filepath process text unix
          xdg-basedir
        ];
        testHaskellDepends = [ base hspec ];
        homepage = "https://github.com/eilvelia/hlesspass";
        description = "Alternative CLI application for LessPass";
        license = lib.licenses.mit;
        mainProgram = "hlesspass";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
